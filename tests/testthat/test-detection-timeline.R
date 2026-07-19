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

test_that("comparison spectrogram renderer can overwrite one current PNG", {
  make_comparison <- function(value) {
    list(
      pieces = list(
        list(
          x_seconds = c(0, 1, 2),
          y_khz = c(0.5, 1, 1.5),
          z = matrix(value + seq_len(9), nrow = 3),
          mic_id = "NCNX06a"
        )
      ),
      origin_time = as.POSIXct("2026-01-01 22:00:00", tz = "UTC"),
      xlim = c(0, 2),
      ylim = c(0.5, 1.5),
      zlim = c(value + 1, value + 9)
    )
  }
  out_path <- tempfile(fileext = ".png")

  write_comparison_spectrogram_png(make_comparison(-90), out_path, width = 400, row_height = 80)
  first_mtime <- file.info(out_path)$mtime
  Sys.sleep(1)
  write_comparison_spectrogram_png(make_comparison(-40), out_path, width = 400, row_height = 80)

  expect_true(file.exists(out_path))
  expect_gt(file.info(out_path)$mtime, first_mtime)
})

test_that("grouped and removed action rows have expected export columns", {
  rows <- data.frame(
    rec_id = c("NCNX06a_1", "NCNX06b_2"),
    detection_id = c(1, 2),
    mic_id = c("NCNX06a", "NCNX06b"),
    toa = as.POSIXct(c("2026-01-01 22:00:00", "2026-01-01 22:00:01"), tz = "UTC"),
    Duration = c(4, 5)
  )

  grouped <- format_detection_action_rows(rows, notes = "same call", group_id = 1)
  removed <- format_detection_action_rows(rows[1, , drop = FALSE], notes = "noise")

  expect_named(grouped, c("group_ID", "detection_ID", "recorder_ID", "detection_start_time", "detection_end_time", "Notes"))
  expect_named(removed, c("detection_ID", "recorder_ID", "detection_start_time", "detection_end_time", "Notes"))
  expect_equal(grouped$group_ID, c(1L, 1L))
  expect_equal(grouped$detection_ID, c("1", "2"))
  expect_equal(grouped$recorder_ID, c("NCNX06a", "NCNX06b"))
  expect_equal(grouped$detection_end_time, rows$toa + rows$Duration)
  expect_equal(removed$Notes, "noise")
})

test_that("click selection toggles detection ids", {
  selected <- character(0)

  selected <- toggle_rec_id_selection(selected, "a")
  selected <- toggle_rec_id_selection(selected, "b")
  selected <- toggle_rec_id_selection(selected, "a")

  expect_equal(selected, "b")
  expect_equal(toggle_rec_id_selection(selected, NA_character_), "b")
})
