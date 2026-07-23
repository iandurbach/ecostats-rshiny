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
    grouping_method = character(0),
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
    grouping_method = character(0),
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

normalize_grouping_method <- function(x, n = length(x), default = "manual") {
  if (is.null(x)) x <- rep(default, n)
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- default
  if (length(x) != n || any(!x %in% c("manual", "automated"))) {
    stop("grouping_method must contain only 'manual' or 'automated'.")
  }
  x
}

format_detection_action_rows <- function(rows, notes = "", group_id = NULL, grouping_method = "manual") {
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
    out <- data.frame(
      group_ID = rep(as.integer(group_id), nrow(out)),
      grouping_method = normalize_grouping_method(rep(grouping_method, nrow(out))),
      out,
      stringsAsFactors = FALSE
    )
  }
  out
}

format_group_membership_rows <- function(rows, notes = "", group_id, grouping_method = "manual") {
  if (nrow(rows) == 0) return(empty_group_membership())
  data.frame(
    group_ID = rep(as.integer(group_id), nrow(rows)),
    rec_id = as.character(rows$rec_id),
    grouping_method = normalize_grouping_method(rep(grouping_method, nrow(rows))),
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
  group_membership$grouping_method <- normalize_grouping_method(
    if ("grouping_method" %in% names(group_membership)) group_membership$grouping_method else NULL,
    nrow(group_membership)
  )
  pieces <- lapply(seq_len(nrow(group_membership)), function(i) {
    member <- group_membership[i, , drop = FALSE]
    rows <- timeline_data[timeline_data$rec_id %in% member$rec_id, , drop = FALSE]
    rows$suspect_bearing <- member$suspect_bearing
    format_detection_action_rows(
      rows,
      notes = member$Notes,
      group_id = member$group_ID,
      grouping_method = member$grouping_method
    )
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

infer_wgs84_utm <- function(lng, lat) {
  lng <- suppressWarnings(as.numeric(lng))
  lat <- suppressWarnings(as.numeric(lat))
  valid <- is.finite(lng) & is.finite(lat)
  if (!any(valid)) {
    stop("Mic data must include at least one finite lat/lng pair for UTM conversion.")
  }
  median_lng <- stats::median(lng[valid])
  median_lat <- stats::median(lat[valid])
  zone <- floor((median_lng + 180) / 6) + 1
  zone <- min(60, max(1, zone))
  northern <- median_lat >= 0
  epsg <- if (northern) 32600 + zone else 32700 + zone
  list(zone = zone, hemisphere = if (northern) "north" else "south", epsg = epsg)
}

lonlat_to_utm <- function(lng, lat, zone = infer_wgs84_utm(lng, lat)$zone, northern = TRUE) {
  lng <- suppressWarnings(as.numeric(lng))
  lat <- suppressWarnings(as.numeric(lat))
  if (any(!is.finite(lng) | !is.finite(lat))) {
    stop("All mic lat/lng values must be finite for UTM conversion.")
  }

  a <- 6378137
  f <- 1 / 298.257223563
  k0 <- 0.9996
  e2 <- f * (2 - f)
  ep2 <- e2 / (1 - e2)
  lat_rad <- lat * pi / 180
  lng_rad <- lng * pi / 180
  lon0 <- ((zone - 1) * 6 - 180 + 3) * pi / 180

  n <- a / sqrt(1 - e2 * sin(lat_rad)^2)
  t <- tan(lat_rad)^2
  c <- ep2 * cos(lat_rad)^2
  aa <- cos(lat_rad) * (lng_rad - lon0)
  m <- a * (
    (1 - e2 / 4 - 3 * e2^2 / 64 - 5 * e2^3 / 256) * lat_rad -
      (3 * e2 / 8 + 3 * e2^2 / 32 + 45 * e2^3 / 1024) * sin(2 * lat_rad) +
      (15 * e2^2 / 256 + 45 * e2^3 / 1024) * sin(4 * lat_rad) -
      (35 * e2^3 / 3072) * sin(6 * lat_rad)
  )

  x <- k0 * n * (
    aa + (1 - t + c) * aa^3 / 6 +
      (5 - 18 * t + t^2 + 72 * c - 58 * ep2) * aa^5 / 120
  ) + 500000
  y <- k0 * (
    m + n * tan(lat_rad) * (
      aa^2 / 2 +
        (5 - t + 9 * c + 4 * c^2) * aa^4 / 24 +
        (61 - 58 * t + t^2 + 600 * c - 330 * ep2) * aa^6 / 720
    )
  )
  if (!northern) y <- y + 10000000

  data.frame(x = x, y = y)
}

prepare_acre_traps <- function(mic_data) {
  required <- c("mic_id", "lat", "lng")
  missing_required <- setdiff(required, names(mic_data))
  if (length(missing_required) > 0) {
    stop(paste("Mic data are missing column(s):", paste(missing_required, collapse = ", ")))
  }

  mics <- unique(mic_data[required])
  if (anyDuplicated(mics$mic_id)) {
    stop("Mic data contain duplicate mic IDs with different coordinates.")
  }
  mics <- mics[order(mics$mic_id), , drop = FALSE]
  utm <- infer_wgs84_utm(mics$lng, mics$lat)
  coords <- lonlat_to_utm(mics$lng, mics$lat, zone = utm$zone, northern = utm$hemisphere == "north")
  traps <- data.frame(x = coords$x, y = coords$y, stringsAsFactors = FALSE)
  lookup <- data.frame(
    trap = seq_len(nrow(mics)),
    mic_id = as.character(mics$mic_id),
    lat = as.numeric(mics$lat),
    lng = as.numeric(mics$lng),
    x = coords$x,
    y = coords$y,
    stringsAsFactors = FALSE
  )
  list(traps = traps, lookup = lookup, utm = utm)
}

build_acre_capture_ids <- function(rows, group_membership) {
  group_lookup <- group_membership[!duplicated(group_membership$rec_id), c("group_ID", "rec_id"), drop = FALSE]
  rows <- dplyr::left_join(rows, group_lookup, by = "rec_id")
  grouped_ids <- sort(unique(rows$group_ID[!is.na(rows$group_ID)]))
  group_id_map <- data.frame(
    group_ID = grouped_ids,
    ID = seq_along(grouped_ids),
    stringsAsFactors = FALSE
  )
  rows <- dplyr::left_join(rows, group_id_map, by = "group_ID")
  singleton <- is.na(rows$ID)
  rows$ID[singleton] <- length(grouped_ids) + seq_len(sum(singleton))
  rows$acre_source <- ifelse(singleton, "singleton", paste0("group_", rows$group_ID))
  rows
}

build_acre_export_bundle <- function(timeline_data, mic_data, sessions, group_membership, removed_membership, source_db_paths = character(0)) {
  required_timeline <- c("rec_id", "mic_id", "toa", "bearing", "session_id")
  missing_timeline <- setdiff(required_timeline, names(timeline_data))
  if (length(missing_timeline) > 0) {
    stop(paste("Timeline data are missing column(s):", paste(missing_timeline, collapse = ", ")))
  }
  required_sessions <- c("session_id", "real_start", "duration_seconds")
  missing_sessions <- setdiff(required_sessions, names(sessions))
  if (length(missing_sessions) > 0) {
    stop(paste("Session data are missing column(s):", paste(missing_sessions, collapse = ", ")))
  }

  active_rows <- timeline_data[!timeline_data$rec_id %in% removed_membership$rec_id, , drop = FALSE]
  if (nrow(active_rows) == 0) {
    stop("No nonremoved detections are available for acre export.")
  }
  if (any(is.na(active_rows$session_id))) {
    stop("Cannot export acre inputs because at least one nonremoved detection is outside a recording session.")
  }
  if (anyDuplicated(group_membership$rec_id)) {
    stop("At least one detection is assigned to more than one group.")
  }
  if (any(group_membership$rec_id %in% removed_membership$rec_id)) {
    stop("At least one detection is both grouped and removed.")
  }

  trap_info <- prepare_acre_traps(mic_data)
  sessions <- sessions[sessions$session_id %in% unique(active_rows$session_id), , drop = FALSE]
  session_lookup <- data.frame(
    session_id = as.character(sessions$session_id),
    session = seq_len(nrow(sessions)),
    real_start = sessions$real_start,
    duration_seconds = as.numeric(sessions$duration_seconds),
    stringsAsFactors = FALSE
  )
  active_rows <- dplyr::left_join(active_rows, session_lookup, by = "session_id")
  active_rows <- dplyr::left_join(active_rows, trap_info$lookup[, c("trap", "mic_id"), drop = FALSE], by = "mic_id")
  if (any(is.na(active_rows$session)) || any(is.na(active_rows$trap))) {
    stop("Could not match all detections to acre sessions and traps.")
  }

  active_rows <- build_acre_capture_ids(active_rows, group_membership)
  captures <- data.frame(
    session = as.integer(active_rows$session),
    ID = as.integer(active_rows$ID),
    trap = as.integer(active_rows$trap),
    bearing = as.numeric(active_rows$bearing) * pi / 180,
    toa = as.numeric(difftime(active_rows$toa, active_rows$real_start, units = "secs")),
    stringsAsFactors = FALSE
  )
  captures <- captures[order(captures$session, captures$ID, captures$trap), , drop = FALSE]

  capture_lookup <- data.frame(
    rec_id = as.character(active_rows$rec_id),
    acre_ID = as.integer(active_rows$ID),
    acre_source = as.character(active_rows$acre_source),
    session = as.integer(active_rows$session),
    trap = as.integer(active_rows$trap),
    stringsAsFactors = FALSE
  )
  capture_lookup <- capture_lookup[order(capture_lookup$session, capture_lookup$acre_ID, capture_lookup$trap), , drop = FALSE]

  metadata <- list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    utm = trap_info$utm,
    trap_lookup = trap_info$lookup,
    capture_lookup = capture_lookup,
    source_db_paths = as.character(source_db_paths),
    included_auxiliary = c("bearing", "toa"),
    omitted_auxiliary = c("dist", "ss"),
    notes = "Acre captures include all nonremoved detections. Grouped detections share an ID; ungrouped detections are singleton IDs."
  )

  list(
    captures = captures,
    traps = trap_info$traps,
    sessions = session_lookup,
    survey.length = session_lookup$duration_seconds,
    metadata = metadata
  )
}

save_acre_export_bundle <- function(bundle, file) {
  captures <- bundle$captures
  traps <- bundle$traps
  sessions <- bundle$sessions
  survey.length <- bundle$survey.length
  metadata <- bundle$metadata
  save(captures, traps, sessions, survey.length, metadata, file = file)
}

acre_script_text <- function(input_file = "acre_inputs.RData", buffer_m = 1000) {
  paste(
    "# Acre model fitting script generated by vocomatcher",
    "",
    "library(acre)",
    "",
    sprintf("input_file <- %s", deparse(input_file)),
    "if (!file.exists(input_file)) input_file <- file.choose()",
    "load(input_file)",
    "",
    "# Review this buffer before serious analysis; it controls the mask around the detector array.",
    sprintf("buffer_m <- %s", format(buffer_m, scientific = FALSE)),
    "",
    "dat <- acre::read.acre(",
    "  captures = captures,",
    "  traps = traps,",
    "  control.mask = list(buffer = buffer_m),",
    "  survey.length = survey.length",
    ")",
    "",
    "fit <- acre::fit.acre(dat, detfn = \"hn\", tracing = TRUE)",
    "print(summary(fit))",
    sep = "\n"
  )
}

timeline_detection_keys <- function(timeline_data) {
  required <- c("rec_id", "mic_id")
  missing_required <- setdiff(required, names(timeline_data))
  if (length(missing_required) > 0) {
    stop(paste("Timeline data are missing column(s):", paste(missing_required, collapse = ", ")))
  }

  detection_ids <- if ("detection_id" %in% names(timeline_data)) timeline_data$detection_id else timeline_data$rec_id
  paste(as.character(timeline_data$mic_id), as.character(detection_ids), sep = "\r")
}

action_detection_keys <- function(rows, data_name) {
  required <- c("detection_ID", "recorder_ID")
  missing_required <- setdiff(required, names(rows))
  if (length(missing_required) > 0) {
    stop(paste(data_name, "are missing column(s):", paste(missing_required, collapse = ", ")))
  }

  paste(as.character(rows$recorder_ID), as.character(rows$detection_ID), sep = "\r")
}

import_group_membership <- function(groups, timeline_data) {
  if (is.null(groups)) return(empty_group_membership())
  if (!is.data.frame(groups)) {
    stop("Loaded groups must be a data frame.")
  }
  if (nrow(groups) == 0) return(empty_group_membership())
  required <- c("group_ID", "detection_ID", "recorder_ID")
  missing_required <- setdiff(required, names(groups))
  if (length(missing_required) > 0) {
    stop(paste("Loaded groups are missing column(s):", paste(missing_required, collapse = ", ")))
  }

  idx <- match(action_detection_keys(groups, "Loaded groups"), timeline_detection_keys(timeline_data))
  if (any(is.na(idx))) {
    stop(paste(sum(is.na(idx)), "loaded grouped detection(s) do not match the current detections."))
  }

  out <- data.frame(
    group_ID = as.integer(groups$group_ID),
    rec_id = as.character(timeline_data$rec_id[idx]),
    grouping_method = normalize_grouping_method(
      if ("grouping_method" %in% names(groups)) groups$grouping_method else NULL,
      nrow(groups)
    ),
    Notes = if ("Notes" %in% names(groups)) as.character(groups$Notes) else rep("", nrow(groups)),
    suspect_bearing = if ("suspect_bearing" %in% names(groups)) as.logical(groups$suspect_bearing) else rep(FALSE, nrow(groups)),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(out$rec_id)) {
    stop("Loaded groups assign at least one detection to more than one group.")
  }
  out
}

import_removed_membership <- function(removed_points, timeline_data) {
  if (is.null(removed_points)) return(empty_removed_membership())
  if (!is.data.frame(removed_points)) {
    stop("Loaded removed points must be a data frame.")
  }
  if (nrow(removed_points) == 0) return(empty_removed_membership())
  required <- c("detection_ID", "recorder_ID")
  missing_required <- setdiff(required, names(removed_points))
  if (length(missing_required) > 0) {
    stop(paste("Loaded removed points are missing column(s):", paste(missing_required, collapse = ", ")))
  }

  idx <- match(action_detection_keys(removed_points, "Loaded removed points"), timeline_detection_keys(timeline_data))
  if (any(is.na(idx))) {
    stop(paste(sum(is.na(idx)), "loaded removed detection(s) do not match the current detections."))
  }

  out <- data.frame(
    rec_id = as.character(timeline_data$rec_id[idx]),
    Notes = if ("Notes" %in% names(removed_points)) as.character(removed_points$Notes) else rep("", nrow(removed_points)),
    suspect_bearing = if ("suspect_bearing" %in% names(removed_points)) as.logical(removed_points$suspect_bearing) else rep(FALSE, nrow(removed_points)),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(out$rec_id)) {
    stop("Loaded removed points contain duplicate detections.")
  }
  out
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

session_group_order <- function(session_rows, group_membership) {
  if (nrow(session_rows) == 0 || nrow(group_membership) == 0) {
    return(data.frame(group_ID = integer(0), median_toa = as.POSIXct(character(0), tz = "UTC")))
  }
  required_rows <- c("rec_id", "toa")
  required_groups <- c("group_ID", "rec_id")
  missing_rows <- setdiff(required_rows, names(session_rows))
  missing_groups <- setdiff(required_groups, names(group_membership))
  if (length(missing_rows) > 0) stop(paste("Session rows are missing column(s):", paste(missing_rows, collapse = ", ")))
  if (length(missing_groups) > 0) stop(paste("Group membership is missing column(s):", paste(missing_groups, collapse = ", ")))

  joined <- merge(
    group_membership[, required_groups, drop = FALSE],
    session_rows[, required_rows, drop = FALSE],
    by = "rec_id"
  )
  joined <- joined[!is.na(joined$group_ID) & !is.na(joined$toa), , drop = FALSE]
  if (nrow(joined) == 0) {
    return(data.frame(group_ID = integer(0), median_toa = as.POSIXct(character(0), tz = "UTC")))
  }

  medians <- stats::aggregate(
    as.numeric(joined$toa),
    by = list(group_ID = joined$group_ID),
    FUN = stats::median
  )
  medians$median_toa <- as.POSIXct(medians$x, origin = "1970-01-01", tz = "UTC")
  medians <- medians[order(medians$median_toa, medians$group_ID), c("group_ID", "median_toa"), drop = FALSE]
  rownames(medians) <- NULL
  medians
}

session_group_ids <- function(session_rows, group_membership) {
  session_group_order(session_rows, group_membership)$group_ID
}

next_session_group_id <- function(current_group_id, direction, session_rows, group_membership) {
  group_ids <- session_group_ids(session_rows, group_membership)
  if (length(group_ids) == 0) return(integer(0))

  direction <- if (direction < 0) -1L else 1L
  current_group_id <- suppressWarnings(as.integer(current_group_id))
  if (length(current_group_id) != 1 || is.na(current_group_id) || !current_group_id %in% group_ids) {
    return(if (direction > 0) group_ids[[1]] else utils::tail(group_ids, 1))
  }

  current_index <- match(current_group_id, group_ids)
  next_index <- ((current_index - 1L + direction) %% length(group_ids)) + 1L
  group_ids[[next_index]]
}

add_session_group_display <- function(session_rows, group_membership) {
  session_rows$group_ID <- NA_integer_
  session_rows$group_label <- ""
  if (nrow(session_rows) == 0 || nrow(group_membership) == 0) return(session_rows)

  order <- session_group_order(session_rows, group_membership)
  if (nrow(order) == 0) return(session_rows)

  display <- data.frame(
    group_ID = order$group_ID,
    group_label = paste0("Group ", order$group_ID),
    stringsAsFactors = FALSE
  )
  lookup <- dplyr::left_join(
    group_membership[, c("group_ID", "rec_id"), drop = FALSE],
    display,
    by = "group_ID"
  )
  out <- dplyr::left_join(session_rows, lookup, by = "rec_id")
  out$group_ID <- out$group_ID.y
  out$group_label <- out$group_label.y
  out$group_ID.x <- NULL
  out$group_ID.y <- NULL
  out$group_label.x <- NULL
  out$group_label.y <- NULL
  out$group_label[is.na(out$group_label)] <- ""
  out
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

automatic_group_review_range <- function(rows, session_row, window_seconds = 120) {
  session_start <- session_row$real_start[[1]]
  session_stop <- session_row$real_stop[[1]]
  if (nrow(rows) == 0) return(list(session_start, session_stop))

  session_span <- as.numeric(difftime(session_stop, session_start, units = "secs"))
  if (!is.finite(session_span) || session_span <= window_seconds) {
    return(list(session_start, session_stop))
  }

  midpoint <- min(rows$toa, na.rm = TRUE) +
    as.numeric(difftime(max(rows$toa, na.rm = TRUE), min(rows$toa, na.rm = TRUE), units = "secs")) / 2
  start_time <- midpoint - window_seconds / 2
  stop_time <- midpoint + window_seconds / 2
  if (start_time < session_start) {
    start_time <- session_start
    stop_time <- session_start + window_seconds
  } else if (stop_time > session_stop) {
    stop_time <- session_stop
    start_time <- session_stop - window_seconds
  }
  list(start_time, stop_time)
}

clear_session_group_membership <- function(group_membership, session_rows) {
  if (nrow(group_membership) == 0 || nrow(session_rows) == 0) return(group_membership)
  group_membership[!group_membership$rec_id %in% session_rows$rec_id, , drop = FALSE]
}

timeline_range_start <- function(x_range, session_row) {
  fallback <- session_row$real_start[[1]]
  if (is.null(x_range) || length(x_range) < 1 || is.null(x_range[[1]])) return(fallback)
  value <- x_range[[1]]
  parsed <- if (inherits(value, "POSIXct")) {
    value
  } else if (is.numeric(value)) {
    as.POSIXct(value, origin = "1970-01-01", tz = "UTC")
  } else {
    as.POSIXct(as.character(value), tz = "UTC")
  }
  if (length(parsed) != 1 || is.na(parsed)) fallback else parsed
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
