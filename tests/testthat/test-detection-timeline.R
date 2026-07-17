test_that("recording intervals are merged into compressed sessions", {
  intervals <- data.frame(
    recording_start_utc = as.POSIXct(
      c("2026-01-01 10:00:00", "2026-01-01 10:59:00", "2026-01-01 22:00:00"),
      tz = "UTC"
    ),
    recording_stop_utc = as.POSIXct(
      c("2026-01-01 11:00:00", "2026-01-01 12:00:00", "2026-01-01 23:00:00"),
      tz = "UTC"
    )
  )

  sessions <- merge_detection_sessions(intervals, gap_seconds = 30 * 60, compressed_gap_seconds = 60)

  expect_equal(nrow(sessions), 2)
  expect_equal(sessions$real_start[[1]], intervals$recording_start_utc[[1]])
  expect_equal(sessions$real_stop[[1]], intervals$recording_stop_utc[[2]])
  expect_equal(sessions$x_start[[1]], 0)
  expect_equal(sessions$x_start[[2]], sessions$duration_seconds[[1]] + 60)
})

test_that("detections are mapped to recorder lanes and compressed time", {
  detections <- data.frame(
    rec_id = c("a1", "b1"),
    mic_id = c("NCNX06b", "NCNX06a"),
    toa = as.POSIXct(c("2026-01-01 10:05:00", "2026-01-01 22:10:00"), tz = "UTC")
  )
  intervals <- data.frame(
    recording_start_utc = as.POSIXct(c("2026-01-01 10:00:00", "2026-01-01 22:00:00"), tz = "UTC"),
    recording_stop_utc = as.POSIXct(c("2026-01-01 12:00:00", "2026-01-01 23:00:00"), tz = "UTC")
  )
  sessions <- merge_detection_sessions(intervals, gap_seconds = 30 * 60, compressed_gap_seconds = 60)

  mapped <- map_detections_to_timeline(detections, sessions)

  expect_equal(attr(mapped, "recorder_levels"), c("NCNX06a", "NCNX06b"))
  expect_equal(mapped$recorder_lane, c(2, 1))
  expect_equal(mapped$x_compressed[[1]], 5 * 60)
  expect_equal(mapped$x_compressed[[2]], sessions$x_start[[2]] + 10 * 60)
})

test_that("brush selection returns detections inside compressed x and recorder lane bounds", {
  timeline_data <- data.frame(
    rec_id = c("a1", "a2", "b1"),
    x_compressed = c(100, 200, 100),
    recorder_lane = c(1, 1, 2)
  )

  selected <- select_timeline_detections(
    timeline_data,
    xmin = 90,
    xmax = 150,
    ymin = 0.5,
    ymax = 1.5
  )

  expect_equal(selected$rec_id, "a1")
})

test_that("recording sessions keep real start and stop times", {
  intervals <- data.frame(
    recording_start_utc = as.POSIXct(
      c("2026-01-01 10:00:00", "2026-01-01 10:59:00", "2026-01-01 22:00:00"),
      tz = "UTC"
    ),
    recording_stop_utc = as.POSIXct(
      c("2026-01-01 11:00:00", "2026-01-01 12:00:00", "2026-01-02 00:00:00"),
      tz = "UTC"
    )
  )

  sessions <- build_recording_sessions(intervals, gap_seconds = 30 * 60)

  expect_equal(nrow(sessions), 2)
  expect_equal(sessions$real_start[[1]], intervals$recording_start_utc[[1]])
  expect_equal(sessions$real_stop[[1]], intervals$recording_stop_utc[[2]])
  expect_equal(sessions$duration_seconds[[2]], 2 * 60 * 60)
})

test_that("detections are assigned to real recording sessions", {
  detections <- data.frame(
    rec_id = c("a1", "b1", "late"),
    mic_id = c("NCNX06a", "NCNX06b", "NCNX06a"),
    toa = as.POSIXct(
      c("2026-01-01 10:05:00", "2026-01-01 22:10:00", "2026-01-02 04:00:00"),
      tz = "UTC"
    )
  )
  intervals <- data.frame(
    recording_start_utc = as.POSIXct(c("2026-01-01 10:00:00", "2026-01-01 22:00:00"), tz = "UTC"),
    recording_stop_utc = as.POSIXct(c("2026-01-01 12:00:00", "2026-01-02 00:00:00"), tz = "UTC")
  )
  sessions <- build_recording_sessions(intervals, gap_seconds = 30 * 60)

  mapped <- assign_detections_to_sessions(detections, sessions)

  expect_equal(mapped$session_id, c("session_1", "session_2", NA_character_))
  expect_equal(mapped$recorder_lane, c(1, 2, 1))
  expect_equal(attr(mapped, "recorder_levels"), c("NCNX06a", "NCNX06b"))
})

test_that("spectrogram matrix dimensions match time and frequency axes", {
  samples <- sin(seq(0, 8 * pi, length.out = 2048))

  spectro <- compute_spectrogram_matrix(
    samples,
    sample_rate = 48000,
    window_size = 512,
    overlap = 0.5,
    freq_min_hz = 100,
    freq_max_hz = 8000
  )

  expect_equal(nrow(spectro$z), length(spectro$x_seconds))
  expect_equal(ncol(spectro$z), length(spectro$y_khz))
  expect_true(min(spectro$y_khz) >= 0.1)
  expect_true(max(spectro$y_khz) <= 8)
})

test_that("comparison spectrogram renderer writes a combined PNG", {
  comparison <- list(
    pieces = list(
      list(
        x_seconds = c(0, 1, 2),
        y_khz = c(0.5, 1, 1.5),
        z = matrix(c(-80, -60, -40, -75, -55, -35, -70, -50, -30), nrow = 3),
        mic_id = "NCNX06a"
      ),
      list(
        x_seconds = c(1, 2, 3),
        y_khz = c(0.5, 1, 1.5),
        z = matrix(c(-78, -58, -38, -73, -53, -33, -68, -48, -28), nrow = 3),
        mic_id = "NCNX06b"
      )
    ),
    origin_time = as.POSIXct("2026-01-01 22:00:00", tz = "UTC"),
    xlim = c(0, 3),
    ylim = c(0.5, 1.5),
    zlim = c(-80, -28)
  )
  out_path <- tempfile(fileext = ".png")

  write_comparison_spectrogram_png(comparison, out_path, width = 500, row_height = 90)

  expect_true(file.exists(out_path))
  expect_gt(file.info(out_path)$size, 0)
})
