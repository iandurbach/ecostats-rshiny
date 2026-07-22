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

  if ("recorder_lane" %in% names(rows)) {
    rows <- rows[order(-rows$recorder_lane, rows$raw_toa), , drop = FALSE]
  } else {
    rows <- rows[order(rows$mic_id, rows$raw_toa, decreasing = TRUE), , drop = FALSE]
  }
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
  file.path(cache_dir, paste0("comparison_", key, "_v2.png"))
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
    bg = "grey82",
    cex.axis = 1.25,
    cex.lab = 1.35
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
      xaxt = "n",
      yaxt = "s",
      cex.axis = 1.25,
      cex.lab = 1.35,
      useRaster = TRUE
    )
    graphics::box()
  }

  axis_at <- graphics::axTicks(1)
  axis_at <- axis_at[axis_at >= comparison$xlim[[1]] & axis_at <= comparison$xlim[[2]]]
  graphics::axis(
    side = 1,
    at = axis_at,
    labels = format(comparison$origin_time + axis_at, "%H:%M:%S", tz = "UTC"),
    outer = TRUE,
    line = 0,
    cex.axis = 1.25
  )
  graphics::mtext(
    "Time (UTC)",
    side = 1,
    outer = TRUE,
    line = 2.2,
    cex = 1.35
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

write_current_comparison_spectrogram_png <- function(rows, wav_root, cache_dir, filename = "current_comparison.png", width = 900, row_height = 150) {
  out_path <- file.path(cache_dir, filename)
  comparison <- prepare_comparison_spectrograms(rows, wav_root)
  write_comparison_spectrogram_png(comparison, out_path, width = width, row_height = row_height)
  out_path
}

empty_grouped_detections <- function() {
  data.frame(
    group_ID = integer(0),
    detection_ID = character(0),
    recorder_ID = character(0),
    detection_start_time = as.POSIXct(character(0), tz = "UTC"),
    detection_end_time = as.POSIXct(character(0), tz = "UTC"),
    Notes = character(0),
    suspect_bearing = logical(0),
    stringsAsFactors = FALSE
  )
}

empty_removed_detections <- function() {
  data.frame(
    detection_ID = character(0),
    recorder_ID = character(0),
    detection_start_time = as.POSIXct(character(0), tz = "UTC"),
    detection_end_time = as.POSIXct(character(0), tz = "UTC"),
    Notes = character(0),
    suspect_bearing = logical(0),
    stringsAsFactors = FALSE
  )
}

empty_group_membership <- function() {
  data.frame(
    group_ID = integer(0),
    rec_id = character(0),
    Notes = character(0),
    suspect_bearing = logical(0),
    stringsAsFactors = FALSE
  )
}

empty_removed_membership <- function() {
  data.frame(
    rec_id = character(0),
    Notes = character(0),
    suspect_bearing = logical(0),
    stringsAsFactors = FALSE
  )
}

format_detection_action_rows <- function(rows, notes = "", group_id = NULL) {
  if (nrow(rows) == 0) {
    if (is.null(group_id)) return(empty_removed_detections())
    return(empty_grouped_detections())
  }
  required <- c("mic_id", "toa", "Duration")
  missing_required <- setdiff(required, names(rows))
  if (length(missing_required) > 0) {
    stop(paste("Selected rows are missing column(s):", paste(missing_required, collapse = ", ")))
  }

  detection_ids <- if ("detection_id" %in% names(rows)) rows$detection_id else rows$rec_id
  out <- data.frame(
    detection_ID = as.character(detection_ids),
    recorder_ID = as.character(rows$mic_id),
    detection_start_time = rows$toa,
    detection_end_time = rows$toa + suppressWarnings(as.numeric(rows$Duration)),
    Notes = rep(as.character(notes %||% ""), nrow(rows)),
    suspect_bearing = if ("suspect_bearing" %in% names(rows)) as.logical(rows$suspect_bearing) else rep(FALSE, nrow(rows)),
    stringsAsFactors = FALSE
  )
  if (!is.null(group_id)) {
    out <- data.frame(group_ID = rep(as.integer(group_id), nrow(out)), out, stringsAsFactors = FALSE)
  }
  out
}

format_group_membership_rows <- function(rows, notes = "", group_id) {
  if (nrow(rows) == 0) return(empty_group_membership())
  data.frame(
    group_ID = rep(as.integer(group_id), nrow(rows)),
    rec_id = as.character(rows$rec_id),
    Notes = rep(as.character(notes %||% ""), nrow(rows)),
    suspect_bearing = if ("suspect_bearing" %in% names(rows)) as.logical(rows$suspect_bearing) else rep(FALSE, nrow(rows)),
    stringsAsFactors = FALSE
  )
}

format_removed_membership_rows <- function(rows, notes = "") {
  if (nrow(rows) == 0) return(empty_removed_membership())
  data.frame(
    rec_id = as.character(rows$rec_id),
    Notes = rep(as.character(notes %||% ""), nrow(rows)),
    suspect_bearing = if ("suspect_bearing" %in% names(rows)) as.logical(rows$suspect_bearing) else rep(FALSE, nrow(rows)),
    stringsAsFactors = FALSE
  )
}

export_grouped_detections <- function(group_membership, timeline_data) {
  if (nrow(group_membership) == 0) return(empty_grouped_detections())
  pieces <- lapply(seq_len(nrow(group_membership)), function(i) {
    member <- group_membership[i, , drop = FALSE]
    rows <- timeline_data[timeline_data$rec_id %in% member$rec_id, , drop = FALSE]
    rows$suspect_bearing <- member$suspect_bearing
    format_detection_action_rows(rows, notes = member$Notes, group_id = member$group_ID)
  })
  dplyr::bind_rows(pieces)
}

export_removed_detections <- function(removed_membership, timeline_data) {
  if (nrow(removed_membership) == 0) return(empty_removed_detections())
  pieces <- lapply(seq_len(nrow(removed_membership)), function(i) {
    member <- removed_membership[i, , drop = FALSE]
    rows <- timeline_data[timeline_data$rec_id %in% member$rec_id, , drop = FALSE]
    rows$suspect_bearing <- member$suspect_bearing
    format_detection_action_rows(rows, notes = member$Notes)
  })
  dplyr::bind_rows(pieces)
}

empty_suspect_bearing_state <- function() {
  data.frame(
    rec_id = character(0),
    suspect_bearing = logical(0),
    stringsAsFactors = FALSE
  )
}

suspect_bearing_for_rec_ids <- function(rec_ids, state, default = FALSE) {
  rec_ids <- as.character(rec_ids)
  if (length(default) == length(rec_ids)) {
    out <- as.logical(default)
  } else {
    default_value <- if (length(default) == 0) FALSE else as.logical(default[[1]])
    out <- rep(default_value, length(rec_ids))
  }
  if (length(rec_ids) == 0 || is.null(state) || nrow(state) == 0) return(as.logical(out))
  idx <- match(rec_ids, state$rec_id)
  matched <- !is.na(idx)
  out[matched] <- state$suspect_bearing[idx[matched]]
  as.logical(out)
}

apply_suspect_bearing_state <- function(rows, state) {
  if (nrow(rows) == 0) return(rows)
  default <- if ("suspect_bearing" %in% names(rows)) as.logical(rows$suspect_bearing) else FALSE
  rows$suspect_bearing <- suspect_bearing_for_rec_ids(rows$rec_id, state, default = default)
  rows
}

toggle_suspect_bearing_state <- function(state, rec_id) {
  rec_id <- as.character(rec_id)
  if (length(rec_id) == 0 || is.na(rec_id[[1]]) || !nzchar(rec_id[[1]])) return(state)
  rec_id <- rec_id[[1]]
  if (is.null(state) || nrow(state) == 0) {
    return(data.frame(rec_id = rec_id, suspect_bearing = TRUE, stringsAsFactors = FALSE))
  }
  state <- state[!duplicated(state$rec_id), , drop = FALSE]
  idx <- match(rec_id, state$rec_id)
  if (is.na(idx)) {
    return(rbind(state, data.frame(rec_id = rec_id, suspect_bearing = TRUE, stringsAsFactors = FALSE)))
  }
  state$suspect_bearing[[idx]] <- !isTRUE(state$suspect_bearing[[idx]])
  state
}

bearing_arrow_color <- function(suspect_bearing = FALSE) {
  ifelse(isTRUE(suspect_bearing), "grey", "red")
}

bearing_rec_id_from_layer_id <- function(layer_id) {
  sub("^arrow_", "", as.character(layer_id))
}

prepare_bearing_arrows <- function(rows, mic_data, suspect_state = empty_suspect_bearing_state(), distance_m = 500) {
  if (nrow(rows) == 0) {
    return(data.frame(
      rec_id = character(0),
      mic_id = character(0),
      bearing = numeric(0),
      suspect_bearing = logical(0),
      lat = numeric(0),
      lng = numeric(0),
      arrow_lat = numeric(0),
      arrow_lng = numeric(0),
      color = character(0),
      stringsAsFactors = FALSE
    ))
  }
  required_rows <- c("rec_id", "mic_id", "bearing")
  required_mics <- c("mic_id", "lat", "lng")
  missing_rows <- setdiff(required_rows, names(rows))
  missing_mics <- setdiff(required_mics, names(mic_data))
  if (length(missing_rows) > 0) stop(paste("Selected rows are missing column(s):", paste(missing_rows, collapse = ", ")))
  if (length(missing_mics) > 0) stop(paste("Mic data are missing column(s):", paste(missing_mics, collapse = ", ")))

  rows <- apply_suspect_bearing_state(rows, suspect_state)
  mic_data <- unique(mic_data[required_mics])
  joined <- dplyr::left_join(rows, mic_data, by = "mic_id")
  joined <- joined[
    !is.na(joined$lat) & !is.na(joined$lng) & !is.na(joined$bearing),
    ,
    drop = FALSE
  ]
  if (nrow(joined) == 0) return(prepare_bearing_arrows(rows[FALSE, , drop = FALSE], mic_data, suspect_state, distance_m))

  endpoints <- lapply(seq_len(nrow(joined)), function(i) {
    point <- geosphere::destPoint(
      p = c(joined$lng[[i]], joined$lat[[i]]),
      b = joined$bearing[[i]],
      d = distance_m
    )
    data.frame(arrow_lng = point[1, 1], arrow_lat = point[1, 2])
  })
  endpoint_df <- dplyr::bind_rows(endpoints)
  joined$arrow_lng <- endpoint_df$arrow_lng
  joined$arrow_lat <- endpoint_df$arrow_lat
  joined$color <- vapply(joined$suspect_bearing, bearing_arrow_color, character(1))
  joined
}

detection_point_status <- function(rec_ids, group_membership, removed_membership) {
  rec_ids <- as.character(rec_ids)
  status <- rep("active", length(rec_ids))
  status[rec_ids %in% group_membership$rec_id] <- "grouped"
  status[rec_ids %in% removed_membership$rec_id] <- "removed"
  status
}

selected_group_ids <- function(rec_ids, group_membership) {
  ids <- unique(group_membership$group_ID[group_membership$rec_id %in% rec_ids])
  ids[order(ids)]
}

group_review_range <- function(rows, session_row, min_window_seconds = 30, padding_seconds = 10, padding_fraction = 0.2) {
  if (nrow(rows) == 0) {
    return(list(session_row$real_start[[1]], session_row$real_stop[[1]]))
  }

  start_time <- min(rows$toa, na.rm = TRUE)
  stop_time <- max(rows$toa, na.rm = TRUE)
  span <- as.numeric(difftime(stop_time, start_time, units = "secs"))
  if (!is.finite(span) || span < min_window_seconds) {
    midpoint <- start_time + span / 2
    start_time <- midpoint - min_window_seconds / 2
    stop_time <- midpoint + min_window_seconds / 2
  } else {
    padding <- max(padding_seconds, span * padding_fraction)
    start_time <- start_time - padding
    stop_time <- stop_time + padding
  }

  list(
    max(start_time, session_row$real_start[[1]]),
    min(stop_time, session_row$real_stop[[1]])
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

toggle_rec_id_selection <- function(current, clicked) {
  current <- unique(as.character(current))
  clicked <- as.character(clicked)
  if (length(clicked) == 0 || is.na(clicked[[1]]) || !nzchar(clicked[[1]])) {
    return(current)
  }
  clicked <- clicked[[1]]
  if (clicked %in% current) {
    setdiff(current, clicked)
  } else {
    c(current, clicked)
  }
}
