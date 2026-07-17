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
