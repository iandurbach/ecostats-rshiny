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
