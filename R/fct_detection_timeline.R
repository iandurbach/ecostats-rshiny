merge_detection_sessions <- function(intervals, gap_seconds = 30 * 60, compressed_gap_seconds = 60) {
  required <- c("recording_start_utc", "recording_stop_utc")
  missing_required <- setdiff(required, names(intervals))
  if (length(missing_required) > 0) {
    stop(paste("Intervals are missing column(s):", paste(missing_required, collapse = ", ")))
  }

  intervals <- intervals[!is.na(intervals$recording_start_utc) & !is.na(intervals$recording_stop_utc), , drop = FALSE]
  if (nrow(intervals) == 0) {
    return(data.frame(
      session_id = character(0),
      real_start = as.POSIXct(character(0), tz = "UTC"),
      real_stop = as.POSIXct(character(0), tz = "UTC"),
      duration_seconds = numeric(0),
      x_start = numeric(0),
      x_end = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  intervals <- unique(intervals[required])
  intervals <- intervals[order(intervals$recording_start_utc, intervals$recording_stop_utc), , drop = FALSE]

  sessions <- list()
  current_start <- intervals$recording_start_utc[[1]]
  current_stop <- intervals$recording_stop_utc[[1]]

  for (i in seq_len(nrow(intervals))[-1]) {
    gap <- as.numeric(difftime(intervals$recording_start_utc[[i]], current_stop, units = "secs"))
    if (!is.na(gap) && gap <= gap_seconds) {
      current_stop <- max(current_stop, intervals$recording_stop_utc[[i]])
    } else {
      sessions[[length(sessions) + 1]] <- list(start = current_start, stop = current_stop)
      current_start <- intervals$recording_start_utc[[i]]
      current_stop <- intervals$recording_stop_utc[[i]]
    }
  }
  sessions[[length(sessions) + 1]] <- list(start = current_start, stop = current_stop)

  out <- data.frame(
    session_id = paste0("session_", seq_along(sessions)),
    real_start = as.POSIXct(vapply(sessions, `[[`, numeric(1), "start"), origin = "1970-01-01", tz = "UTC"),
    real_stop = as.POSIXct(vapply(sessions, `[[`, numeric(1), "stop"), origin = "1970-01-01", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  out$duration_seconds <- as.numeric(difftime(out$real_stop, out$real_start, units = "secs"))
  out$x_start <- c(0, cumsum(utils::head(out$duration_seconds, -1) + compressed_gap_seconds))
  out$x_end <- out$x_start + out$duration_seconds
  out
}

map_detections_to_timeline <- function(detections, sessions, time_col = "toa", recorder_col = "mic_id") {
  required <- c("rec_id", time_col, recorder_col)
  missing_required <- setdiff(required, names(detections))
  if (length(missing_required) > 0) {
    stop(paste("Detections are missing column(s):", paste(missing_required, collapse = ", ")))
  }

  out <- detections
  out$session_id <- NA_character_
  out$x_compressed <- NA_real_

  for (i in seq_len(nrow(sessions))) {
    in_session <- out[[time_col]] >= sessions$real_start[[i]] & out[[time_col]] <= sessions$real_stop[[i]]
    out$session_id[in_session] <- sessions$session_id[[i]]
    out$x_compressed[in_session] <- sessions$x_start[[i]] +
      as.numeric(difftime(out[[time_col]][in_session], sessions$real_start[[i]], units = "secs"))
  }

  recorder_levels <- sort(unique(as.character(out[[recorder_col]])))
  out$recorder_lane <- match(as.character(out[[recorder_col]]), recorder_levels)
  attr(out, "recorder_levels") <- recorder_levels
  out
}

format_timeline_time <- function(x, sessions) {
  vapply(x, function(value) {
    idx <- which(value >= sessions$x_start & value <= sessions$x_end)
    if (length(idx) == 0) return("")
    i <- idx[[1]]
    format(sessions$real_start[[i]] + (value - sessions$x_start[[i]]), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }, character(1))
}

select_timeline_detections <- function(timeline_data, xmin, xmax, ymin, ymax) {
  selected <- !is.na(timeline_data$x_compressed) &
    timeline_data$x_compressed >= xmin &
    timeline_data$x_compressed <= xmax &
    timeline_data$recorder_lane >= ymin &
    timeline_data$recorder_lane <= ymax
  timeline_data[selected, , drop = FALSE]
}

read_recording_intervals_from_databases <- function(db_paths) {
  pieces <- lapply(db_paths, function(db_path) {
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    intervals <- pair_sound_acquisition_intervals(DBI::dbReadTable(conn, "Sound_Acquisition"))
    intervals$mic_id <- parse_recorder_id_from_database_path(db_path)
    intervals$database_path <- normalizePath(db_path, mustWork = FALSE)
    intervals
  })

  dplyr::bind_rows(pieces)
}

build_recording_sessions <- function(intervals, gap_seconds = 30 * 60) {
  sessions <- merge_detection_sessions(
    intervals,
    gap_seconds = gap_seconds,
    compressed_gap_seconds = 0
  )
  sessions[, c("session_id", "real_start", "real_stop", "duration_seconds"), drop = FALSE]
}

assign_detections_to_sessions <- function(detections, sessions, time_col = "toa", recorder_col = "mic_id") {
  required <- c("rec_id", time_col, recorder_col)
  missing_required <- setdiff(required, names(detections))
  if (length(missing_required) > 0) {
    stop(paste("Detections are missing column(s):", paste(missing_required, collapse = ", ")))
  }

  out <- detections
  out$session_id <- NA_character_
  for (i in seq_len(nrow(sessions))) {
    in_session <- out[[time_col]] >= sessions$real_start[[i]] & out[[time_col]] <= sessions$real_stop[[i]]
    out$session_id[in_session] <- sessions$session_id[[i]]
  }

  recorder_levels <- sort(unique(as.character(out[[recorder_col]])))
  out$recorder_lane <- match(as.character(out[[recorder_col]]), recorder_levels)
  attr(out, "recorder_levels") <- recorder_levels
  out
}

compute_spectrogram_matrix <- function(samples, sample_rate, window_size = 1024, overlap = 0.75, freq_min_hz = 100, freq_max_hz = 8000) {
  hop <- max(1, as.integer(window_size * (1 - overlap)))
  if (length(samples) < window_size) {
    samples <- c(samples, rep(0, window_size - length(samples)))
  }
  starts <- seq(1, length(samples) - window_size + 1, by = hop)
  window <- 0.5 - 0.5 * cos(2 * pi * (seq_len(window_size) - 1) / (window_size - 1))
  spec <- vapply(starts, function(start) {
    chunk <- samples[start:(start + window_size - 1)] * window
    fft_values <- abs(stats::fft(chunk))[seq_len(window_size / 2)]
    20 * log10(fft_values + 1e-8)
  }, numeric(window_size / 2))

  freqs <- seq(0, sample_rate / 2, length.out = nrow(spec))
  keep <- freqs >= freq_min_hz & freqs <= freq_max_hz
  list(
    x_seconds = seq_along(starts) * hop / sample_rate,
    y_khz = freqs[keep] / 1000,
    z = t(spec[keep, , drop = FALSE])
  )
}

prepare_comparison_spectrograms <- function(rows, wav_root, window_size = 1024, overlap = 0.75, freq_min_hz = 100, freq_max_hz = 8000) {
  if (nrow(rows) == 0) {
    stop("No detections selected.")
  }
  required <- c("rec_id", "mic_id", "raw_toa", "start_frame", "end_frame", "wav_file")
  missing_required <- setdiff(required, names(rows))
  if (length(missing_required) > 0) {
    stop(paste("Selected rows are missing column(s):", paste(missing_required, collapse = ", ")))
  }

  rows <- rows[order(rows$raw_toa, rows$mic_id), , drop = FALSE]
  origin_time <- min(rows$raw_toa, na.rm = TRUE)
  pieces <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    wav_path <- resolve_detection_wav_path(row, wav_root)
    if (is.na(wav_path) || !file.exists(wav_path)) {
      stop(paste("WAV file not found:", wav_path))
    }
    segment <- read_wav_pcm_segment(wav_path, row$start_frame[[1]], row$end_frame[[1]])
    spectro <- compute_spectrogram_matrix(
      segment$samples,
      segment$sample_rate,
      window_size = window_size,
      overlap = overlap,
      freq_min_hz = freq_min_hz,
      freq_max_hz = freq_max_hz
    )
    spectro$x_seconds <- spectro$x_seconds + as.numeric(difftime(row$raw_toa[[1]], origin_time, units = "secs"))
    spectro$rec_id <- row$rec_id[[1]]
    spectro$mic_id <- row$mic_id[[1]]
    spectro$raw_toa <- row$raw_toa[[1]]
    spectro
  })

  z_values <- unlist(lapply(pieces, function(piece) as.vector(piece$z)), use.names = FALSE)
  x_max <- max(vapply(pieces, function(piece) max(piece$x_seconds), numeric(1)))
  list(
    pieces = pieces,
    origin_time = origin_time,
    xlim = c(0, x_max),
    ylim = c(freq_min_hz, freq_max_hz) / 1000,
    zlim = range(z_values, finite = TRUE)
  )
}

comparison_spectrogram_cache_path <- function(rows, cache_dir) {
  key <- paste(rows$rec_id[order(rows$rec_id)], collapse = "_")
  key <- gsub("[^A-Za-z0-9_]+", "_", key)
  file.path(cache_dir, paste0("comparison_", key, "_v1.png"))
}

write_comparison_spectrogram_png <- function(comparison, out_path, width = 900, row_height = 150) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  pieces <- comparison$pieces
  n <- length(pieces)
  grDevices::png(out_path, width = width, height = max(220, row_height * n + 70), bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(
    mfrow = c(n, 1),
    mar = c(0.4, 4.8, 0.2, 0.8),
    oma = c(3.6, 0, 0.2, 0),
    bg = "grey82"
  )

  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    graphics::par(bg = "grey82")
    graphics::image(
      x = piece$x_seconds,
      y = piece$y_khz,
      z = piece$z,
      xlim = comparison$xlim,
      ylim = comparison$ylim,
      zlim = comparison$zlim,
      col = hcl.colors(128, "Inferno"),
      xlab = "",
      ylab = piece$mic_id,
      xaxt = if (i == n) "s" else "n",
      yaxt = "s",
      useRaster = TRUE
    )
    graphics::box()
  }

  graphics::mtext(
    paste0("Seconds since ", format(comparison$origin_time, "%H:%M:%S", tz = "UTC")),
    side = 1,
    outer = TRUE,
    line = 2.2
  )
  invisible(out_path)
}

ensure_comparison_spectrogram_png <- function(rows, wav_root, cache_dir) {
  out_path <- comparison_spectrogram_cache_path(rows, cache_dir)
  if (file.exists(out_path)) return(out_path)
  comparison <- prepare_comparison_spectrograms(rows, wav_root)
  write_comparison_spectrogram_png(comparison, out_path)
  out_path
}
