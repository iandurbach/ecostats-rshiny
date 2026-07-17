parse_recorder_id_from_database_path <- function(path) {
  stem <- basename(path)
  stem <- sub("\\.sqlite3$", "", stem, ignore.case = TRUE)
  sub("_database$", "", stem, ignore.case = TRUE)
}

parse_cluster_id_from_mic_id <- function(mic_id) {
  match <- regexpr("[0-9]+", mic_id)
  ifelse(match > 0, regmatches(mic_id, match), NA_character_)
}

normalize_bearing_degrees <- function(x) {
  ((x %% 360) + 360) %% 360
}

median_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else stats::median(x)
}

read_database_table <- function(conn, table_name) {
  DBI::dbReadTable(conn, table_name)
}

resolve_detection_table <- function(conn) {
  tables <- DBI::dbListTables(conn)
  if ("Gibbon_A" %in% tables) return("Gibbon_A")
  if ("GibbonA" %in% tables) return("GibbonA")
  stop("Database is missing detection table Gibbon_A/GibbonA.")
}

parse_db_utc <- function(x) {
  as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
}

read_uint32_le <- function(con) {
  bytes <- readBin(con, "raw", 4)
  if (length(bytes) < 4) stop("Unexpected end of file while reading WAV header.")
  sum(as.numeric(bytes) * 256^(0:3))
}

nearest_time_offsets <- function(detection_times, offset_times, offsets) {
  if (length(offset_times) == 0 || all(is.na(offset_times))) {
    return(rep(0, length(detection_times)))
  }

  vapply(detection_times, function(t) {
    diffs <- abs(as.numeric(difftime(offset_times, t, units = "secs")))
    idx <- which.min(diffs)
    if (length(idx) == 0 || is.na(idx)) 0 else offsets[[idx]]
  }, numeric(1))
}

pair_sound_acquisition_intervals <- function(sound_acquisition) {
  required <- c("UTC", "Status", "SystemName", "Samples", "sampleRate")
  missing_required <- setdiff(required, names(sound_acquisition))
  if (length(missing_required) > 0) {
    stop(paste("Sound_Acquisition is missing column(s):", paste(missing_required, collapse = ", ")))
  }

  sound_acquisition$utc_parsed <- parse_db_utc(sound_acquisition$UTC)
  sound_acquisition$Status <- trimws(as.character(sound_acquisition$Status))
  sound_acquisition$SystemName <- trimws(as.character(sound_acquisition$SystemName))
  starts <- sound_acquisition[sound_acquisition$Status == "Start", , drop = FALSE]
  stops <- sound_acquisition[sound_acquisition$Status == "Stop", , drop = FALSE]

  empty_intervals <- data.frame(
    wav_file = character(0),
    recording_start_utc = as.POSIXct(character(0), tz = "UTC"),
    recording_stop_utc = as.POSIXct(character(0), tz = "UTC"),
    samples = numeric(0),
    sample_rate = numeric(0),
    stringsAsFactors = FALSE
  )
  if (nrow(starts) == 0 || nrow(stops) == 0) return(empty_intervals)

  out <- lapply(seq_len(nrow(starts)), function(i) {
    start_row <- starts[i, , drop = FALSE]
    matching_stops <- stops[
      stops$SystemName == start_row$SystemName[[1]] &
        stops$utc_parsed >= start_row$utc_parsed[[1]],
      ,
      drop = FALSE
    ]
    if (nrow(matching_stops) == 0) return(NULL)
    stop_row <- matching_stops[which.min(matching_stops$utc_parsed), , drop = FALSE]
    data.frame(
      wav_file = start_row$SystemName[[1]],
      recording_start_utc = start_row$utc_parsed[[1]],
      recording_stop_utc = stop_row$utc_parsed[[1]],
      samples = suppressWarnings(as.numeric(stop_row$Samples[[1]])),
      sample_rate = suppressWarnings(as.numeric(start_row$sampleRate[[1]])),
      stringsAsFactors = FALSE
    )
  })

  if (length(out) == 0 || all(vapply(out, is.null, logical(1)))) {
    return(empty_intervals)
  }
  dplyr::bind_rows(out)
}

match_detections_to_recordings <- function(detections, intervals) {
  if (nrow(detections) == 0) return(detections)
  matched <- lapply(seq_len(nrow(detections)), function(i) {
    det <- detections[i, , drop = FALSE]
    candidates <- intervals[
      intervals$recording_start_utc <= det$raw_toa[[1]] &
        intervals$recording_stop_utc >= det$raw_toa[[1]],
      ,
      drop = FALSE
    ]
    if (nrow(candidates) == 0) {
      det$wav_file <- NA_character_
      det$recording_start_utc <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
      det$recording_stop_utc <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
      det$samples <- NA_real_
      det$sample_rate <- NA_real_
      det$start_frame <- NA_real_
      det$end_frame <- NA_real_
      return(det)
    }

    rec <- candidates[which.min(candidates$recording_start_utc), , drop = FALSE]
    start_frame <- as.numeric(difftime(det$raw_toa[[1]], rec$recording_start_utc[[1]], units = "secs")) * rec$sample_rate[[1]]
    det$wav_file <- rec$wav_file[[1]]
    det$recording_start_utc <- rec$recording_start_utc[[1]]
    det$recording_stop_utc <- rec$recording_stop_utc[[1]]
    det$samples <- rec$samples[[1]]
    det$sample_rate <- rec$sample_rate[[1]]
    det$start_frame <- max(0, floor(start_frame))
    det$end_frame <- ceiling(det$start_frame[[1]] + det$Duration[[1]] * rec$sample_rate[[1]])
    det
  })

  dplyr::bind_rows(matched)
}

read_single_detection_database <- function(db_path, offset_sign = 1) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  detection_table <- resolve_detection_table(conn)
  detections <- read_database_table(conn, detection_table)
  locations <- read_database_table(conn, "Trex_Location")
  orientations <- read_database_table(conn, "Trex_Orientation")
  offsets <- read_database_table(conn, "Trex_Time_Offset")
  sound_acquisition <- read_database_table(conn, "Sound_Acquisition")

  required_detection <- c("Id", "UTC", "Duration", "BearingAngle1")
  missing_detection <- setdiff(required_detection, names(detections))
  if (length(missing_detection) > 0) {
    stop(paste("Detection table is missing column(s):", paste(missing_detection, collapse = ", ")))
  }

  mic_id <- parse_recorder_id_from_database_path(db_path)
  cluster_id <- parse_cluster_id_from_mic_id(mic_id)
  recorder_heading <- median_or_na(orientations$True_Head)
  mic_row <- data.frame(
    mic_id = mic_id,
    cluster_id = cluster_id,
    lng = median_or_na(locations$Longitude),
    lat = median_or_na(locations$Latitude),
    recorder_heading = recorder_heading,
    database_path = normalizePath(db_path, mustWork = FALSE),
    stringsAsFactors = FALSE
  )

  raw_toa <- parse_db_utc(detections$UTC)
  offset_times <- parse_db_utc(offsets$UTC)
  applied_offsets <- nearest_time_offsets(raw_toa, offset_times, suppressWarnings(as.numeric(offsets$TimeOffset)))
  standardized_toa <- raw_toa + offset_sign * applied_offsets
  intervals <- pair_sound_acquisition_intervals(sound_acquisition)

  rec_rows <- data.frame(
    recording_ID = paste0(mic_id, "_", detections$Id),
    mic_ID = mic_id,
    GPSDatetime2 = format(standardized_toa, "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
    measured_bearing = normalize_bearing_degrees(recorder_heading + detections$BearingAngle1 * 180 / pi),
    measured_gender = NA_character_,
    spectrogram = paste0(mic_id, "_", detections$Id, ".png"),
    database_path = normalizePath(db_path, mustWork = FALSE),
    detection_table = detection_table,
    detection_id = detections$Id,
    cluster_id = cluster_id,
    raw_toa = raw_toa,
    clock_offset = applied_offsets,
    duration = suppressWarnings(as.numeric(detections$Duration)),
    relative_bearing_rad = suppressWarnings(as.numeric(detections$BearingAngle1)),
    vertical_bearing_rad = if ("BearingAngle2" %in% names(detections)) suppressWarnings(as.numeric(detections$BearingAngle2)) else NA_real_,
    recorder_heading = recorder_heading,
    stringsAsFactors = FALSE
  )

  names(rec_rows)[names(rec_rows) == "duration"] <- "Duration"
  rec_rows <- match_detections_to_recordings(rec_rows, intervals)

  list(micData = mic_row, recData = rec_rows)
}

read_detection_databases <- function(db_paths, offset_sign = 1) {
  pieces <- lapply(db_paths, read_single_detection_database, offset_sign = offset_sign)
  list(
    micData = dplyr::bind_rows(lapply(pieces, `[[`, "micData")),
    recData = dplyr::bind_rows(lapply(pieces, `[[`, "recData"))
  )
}

resolve_detection_wav_path <- function(row, wav_root) {
  if (is.null(wav_root) || is.na(row$wav_file[[1]]) || is.na(row$mic_id[[1]])) {
    return(NA_character_)
  }
  file.path(wav_root, row$mic_id[[1]], row$wav_file[[1]])
}

spectrogram_cache_path <- function(row, cache_dir) {
  file.path(cache_dir, paste0(row$rec_id[[1]], "_display_v2.png"))
}

read_wav_pcm_segment <- function(wav_path, start_frame, end_frame) {
  con <- file(wav_path, "rb")
  on.exit(close(con), add = TRUE)

  if (rawToChar(readBin(con, "raw", 4)) != "RIFF") stop("Unsupported WAV: missing RIFF header.")
  read_uint32_le(con)
  if (rawToChar(readBin(con, "raw", 4)) != "WAVE") stop("Unsupported WAV: missing WAVE header.")

  fmt <- NULL
  data_pos <- NULL
  data_size <- NULL

  repeat {
    chunk_id_raw <- readBin(con, "raw", 4)
    if (length(chunk_id_raw) < 4) break
    chunk_id <- rawToChar(chunk_id_raw)
    chunk_size <- read_uint32_le(con)
    chunk_start <- seek(con)

    if (chunk_id == "fmt ") {
      fmt <- list(
        audio_format = readBin(con, "integer", 1, size = 2, endian = "little", signed = FALSE),
        channels = readBin(con, "integer", 1, size = 2, endian = "little", signed = FALSE),
        sample_rate = read_uint32_le(con)
      )
      read_uint32_le(con)
      readBin(con, "integer", 1, size = 2, endian = "little", signed = FALSE)
      fmt$bits_per_sample <- readBin(con, "integer", 1, size = 2, endian = "little", signed = FALSE)
    } else if (chunk_id == "data") {
      data_pos <- seek(con)
      data_size <- chunk_size
      break
    }

    seek(con, where = chunk_start + chunk_size + (chunk_size %% 2), origin = "start")
  }

  if (is.null(fmt) || is.null(data_pos)) stop("Unsupported WAV: missing fmt/data chunk.")
  if (fmt$audio_format != 1) stop("Unsupported WAV: only PCM WAV files are supported.")
  if (!fmt$bits_per_sample %in% c(16, 24, 32)) stop("Unsupported WAV bit depth.")

  bytes_per_sample <- fmt$bits_per_sample / 8
  total_frames <- floor(data_size / (bytes_per_sample * fmt$channels))
  start_frame <- max(0, min(as.integer(start_frame), total_frames))
  end_frame <- max(start_frame + 1, min(as.integer(end_frame), total_frames))
  n_frames <- end_frame - start_frame

  seek(con, where = data_pos + start_frame * bytes_per_sample * fmt$channels, origin = "start")
  values <- readBin(
    con,
    "integer",
    n = n_frames * fmt$channels,
    size = bytes_per_sample,
    endian = "little",
    signed = TRUE
  )
  # always reads channel 2 -- maybe change this
  values <- matrix(values, ncol = fmt$channels, byrow = TRUE)[, 2]
  scale <- 2^(fmt$bits_per_sample - 1)
  list(samples = values / scale, sample_rate = fmt$sample_rate)
}

write_spectrogram_png <- function(
    samples,
    sample_rate,
    out_path,
    detection_time = NULL,
    title = NULL,
    window_size = 1024,
    overlap = 0.75,
    freq_min_hz = 100,
    freq_max_hz = 8000
) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
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
  grDevices::png(out_path, width = 900, height = 320, bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(4, 4, 3, 1))
  plot_times <- seq_along(starts) * hop / sample_rate
  axis_at <- pretty(plot_times)
  axis_at <- axis_at[axis_at >= min(plot_times) & axis_at <= max(plot_times)]
  graphics::image(
    x = plot_times,
    y = freqs[keep] / 1000,
    z = t(spec[keep, , drop = FALSE]),
    col = hcl.colors(128, "Inferno"),
    xlab = "Time",
    ylab = "Frequency (kHz)",
    main = title,
    xaxt = if (is.null(detection_time)) "s" else "n",
    useRaster = TRUE
  )
  if (!is.null(detection_time)) {
    graphics::axis(
      side = 1,
      at = axis_at,
      labels = format(detection_time + axis_at, "%H:%M:%S", tz = "UTC")
    )
  }
  invisible(out_path)
}

ensure_spectrogram_png <- function(row, wav_root, cache_dir) {
  out_path <- spectrogram_cache_path(row, cache_dir)
  if (file.exists(out_path)) return(out_path)

  wav_path <- resolve_detection_wav_path(row, wav_root)
  if (is.na(wav_path) || !file.exists(wav_path)) {
    stop(paste("WAV file not found:", wav_path))
  }
  segment <- read_wav_pcm_segment(wav_path, row$start_frame[[1]], row$end_frame[[1]])
  title <- paste(row$mic_id[[1]], format(row$raw_toa[[1]], "%Y-%m-%d", tz = "UTC"), sep = " | ")
  write_spectrogram_png(
    segment$samples,
    segment$sample_rate,
    out_path,
    detection_time = row$raw_toa[[1]],
    title = title
  )
  out_path
}
