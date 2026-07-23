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
    Duration = c(4, 5),
    suspect_bearing = c(FALSE, TRUE)
  )

  grouped <- format_detection_action_rows(rows, notes = "same call", group_id = 1)
  removed <- format_detection_action_rows(rows[1, , drop = FALSE], notes = "noise")

  expect_named(grouped, c("group_ID", "detection_ID", "recorder_ID", "detection_start_time", "detection_end_time", "Notes", "suspect_bearing"))
  expect_named(removed, c("detection_ID", "recorder_ID", "detection_start_time", "detection_end_time", "Notes", "suspect_bearing"))
  expect_equal(grouped$group_ID, c(1L, 1L))
  expect_equal(grouped$detection_ID, c("1", "2"))
  expect_equal(grouped$recorder_ID, c("NCNX06a", "NCNX06b"))
  expect_equal(grouped$detection_end_time, rows$toa + rows$Duration)
  expect_equal(grouped$suspect_bearing, c(FALSE, TRUE))
  expect_equal(removed$Notes, "noise")
})

test_that("group membership exports keep the requested group data frame shape", {
  rows <- data.frame(
    rec_id = c("NCNX06a_1", "NCNX06b_2"),
    detection_id = c(1, 2),
    mic_id = c("NCNX06a", "NCNX06b"),
    toa = as.POSIXct(c("2026-01-01 22:00:00", "2026-01-01 22:00:01"), tz = "UTC"),
    Duration = c(4, 5),
    suspect_bearing = c(FALSE, TRUE)
  )
  membership <- format_group_membership_rows(rows, notes = "same call", group_id = 3)

  grouped <- export_grouped_detections(membership, rows)

  expect_named(grouped, c("group_ID", "detection_ID", "recorder_ID", "detection_start_time", "detection_end_time", "Notes", "suspect_bearing"))
  expect_equal(grouped$group_ID, c(3L, 3L))
  expect_equal(grouped$Notes, c("same call", "same call"))
  expect_equal(grouped$suspect_bearing, c(FALSE, TRUE))
})

test_that("exported grouped rows import back to group membership", {
  rows <- data.frame(
    rec_id = c("NCNX06a_1", "NCNX06b_2", "NCNX06c_3"),
    detection_id = c(1, 2, 3),
    mic_id = c("NCNX06a", "NCNX06b", "NCNX06c"),
    toa = as.POSIXct(c("2026-01-01 22:00:00", "2026-01-01 22:00:01", "2026-01-01 22:00:30"), tz = "UTC"),
    Duration = c(4, 5, 6),
    suspect_bearing = c(FALSE, TRUE, FALSE)
  )
  membership <- format_group_membership_rows(rows[1:2, , drop = FALSE], notes = "same call", group_id = 3)
  grouped <- export_grouped_detections(membership, rows)

  imported <- import_group_membership(grouped, rows)

  expect_equal(imported$group_ID, c(3L, 3L))
  expect_equal(imported$rec_id, c("NCNX06a_1", "NCNX06b_2"))
  expect_equal(imported$Notes, c("same call", "same call"))
  expect_equal(imported$suspect_bearing, c(FALSE, TRUE))
})

test_that("exported removed rows import back to removed membership", {
  rows <- data.frame(
    rec_id = c("NCNX06a_1", "NCNX06b_2"),
    detection_id = c(1, 2),
    mic_id = c("NCNX06a", "NCNX06b"),
    toa = as.POSIXct(c("2026-01-01 22:00:00", "2026-01-01 22:00:01"), tz = "UTC"),
    Duration = c(4, 5),
    suspect_bearing = c(FALSE, TRUE)
  )
  removed <- format_detection_action_rows(rows[2, , drop = FALSE], notes = "noise")

  imported <- import_removed_membership(removed, rows)

  expect_equal(imported$rec_id, "NCNX06b_2")
  expect_equal(imported$Notes, "noise")
  expect_true(imported$suspect_bearing)
})

test_that("UTM conversion auto-selects WGS84 EPSG and returns metre coordinates", {
  north <- infer_wgs84_utm(lng = c(104.5, 104.6), lat = c(18.1, 18.2))
  south <- infer_wgs84_utm(lng = c(104.5, 104.6), lat = c(-18.1, -18.2))

  expect_equal(north$zone, 48)
  expect_equal(north$epsg, 32648)
  expect_equal(south$epsg, 32748)

  coords <- lonlat_to_utm(lng = 105, lat = 0, zone = 48, northern = TRUE)
  expect_equal(coords$x, 500000, tolerance = 1)
  expect_equal(coords$y, 0, tolerance = 1)
})

test_that("acre export bundle includes groups and singleton nonremoved detections", {
  timeline <- data.frame(
    rec_id = c("a1", "b1", "c1", "d1"),
    mic_id = c("NCNX06a", "NCNX06b", "NCNX06a", "NCNX06b"),
    toa = as.POSIXct(
      c("2026-01-01 22:00:05", "2026-01-01 22:00:07", "2026-01-01 22:01:00", "2026-01-01 22:01:30"),
      tz = "UTC"
    ),
    bearing = c(90, 180, 270, 0),
    session_id = c("session_1", "session_1", "session_1", "session_1"),
    stringsAsFactors = FALSE
  )
  mics <- data.frame(
    mic_id = c("NCNX06b", "NCNX06a"),
    lat = c(18.2, 18.1),
    lng = c(104.6, 104.5),
    stringsAsFactors = FALSE
  )
  sessions <- data.frame(
    session_id = c("session_1", "session_2"),
    real_start = as.POSIXct(c("2026-01-01 22:00:00", "2026-01-02 22:00:00"), tz = "UTC"),
    duration_seconds = c(3600, 7200),
    stringsAsFactors = FALSE
  )
  grouped <- data.frame(
    group_ID = c(3L, 3L),
    rec_id = c("a1", "b1"),
    Notes = "",
    suspect_bearing = FALSE,
    stringsAsFactors = FALSE
  )
  removed <- data.frame(
    rec_id = "d1",
    Notes = "",
    suspect_bearing = FALSE,
    stringsAsFactors = FALSE
  )

  bundle <- build_acre_export_bundle(timeline, mics, sessions, grouped, removed, source_db_paths = "one.sqlite3")

  expect_named(bundle$captures, c("session", "ID", "trap", "bearing", "toa"))
  expect_false(any(c("dist", "ss") %in% names(bundle$captures)))
  expect_equal(nrow(bundle$captures), 3)
  expect_equal(bundle$captures$ID[bundle$captures$trap %in% c(1L, 2L)][1:2], c(1L, 1L))
  expect_equal(sort(unique(bundle$captures$ID)), c(1L, 2L))
  expect_equal(bundle$captures$toa, c(5, 7, 60))
  expect_equal(bundle$captures$bearing, c(pi / 2, pi, 3 * pi / 2), tolerance = 1e-8)
  expect_equal(bundle$metadata$utm$epsg, 32648)
  expect_equal(bundle$metadata$source_db_paths, "one.sqlite3")
  expect_equal(bundle$survey.length, 3600)
  expect_equal(nrow(bundle$sessions), 1)
})

test_that("acre export bundle can be saved as an RData bundle", {
  bundle <- list(
    captures = data.frame(session = 1L, ID = 1L, trap = 1L, bearing = 0, toa = 0),
    traps = data.frame(x = 500000, y = 0),
    sessions = data.frame(session_id = "session_1", session = 1L, duration_seconds = 60),
    survey.length = 60,
    metadata = list(utm = list(epsg = 32648))
  )
  out_path <- tempfile(fileext = ".RData")

  save_acre_export_bundle(bundle, out_path)
  env <- new.env(parent = emptyenv())
  load(out_path, envir = env)

  expect_true(exists("captures", envir = env, inherits = FALSE))
  expect_true(exists("traps", envir = env, inherits = FALSE))
  expect_true(exists("sessions", envir = env, inherits = FALSE))
  expect_true(exists("survey.length", envir = env, inherits = FALSE))
  expect_true(exists("metadata", envir = env, inherits = FALSE))
})

test_that("acre script reads exported inputs and fits a basic half-normal model", {
  script <- acre_script_text()

  expect_match(script, "library\\(acre\\)")
  expect_match(script, "acre::read.acre")
  expect_match(script, "control.mask = list\\(buffer = buffer_m\\)")
  expect_match(script, "acre::fit.acre\\(dat, detfn = \"hn\"")
  expect_match(script, "buffer_m <- 1000")
})

test_that("removed membership exports keep the requested removed data frame shape", {
  rows <- data.frame(
    rec_id = c("NCNX06a_1", "NCNX06b_2"),
    detection_id = c(1, 2),
    mic_id = c("NCNX06a", "NCNX06b"),
    toa = as.POSIXct(c("2026-01-01 22:00:00", "2026-01-01 22:00:01"), tz = "UTC"),
    Duration = c(4, 5),
    suspect_bearing = c(FALSE, TRUE)
  )
  membership <- format_removed_membership_rows(rows[2, , drop = FALSE], notes = "noise")

  removed <- export_removed_detections(membership, rows)

  expect_named(removed, c("detection_ID", "recorder_ID", "detection_start_time", "detection_end_time", "Notes", "suspect_bearing"))
  expect_equal(removed$detection_ID, "2")
  expect_equal(removed$Notes, "noise")
  expect_true(removed$suspect_bearing[[1]])
})

test_that("suspect bearing state toggles and applies by detection id", {
  state <- empty_suspect_bearing_state()

  state <- toggle_suspect_bearing_state(state, "a")
  expect_true(suspect_bearing_for_rec_ids("a", state))
  state <- toggle_suspect_bearing_state(state, "a")
  expect_false(suspect_bearing_for_rec_ids("a", state))
  expect_equal(suspect_bearing_for_rec_ids(c("a", "b"), state, default = c(TRUE, FALSE)), c(FALSE, FALSE))
})

test_that("bearing arrows are prepared from selected detections and recorder coordinates", {
  rows <- data.frame(
    rec_id = c("a1", "b1"),
    mic_id = c("NCNX06a", "NCNX06b"),
    bearing = c(90, 180),
    stringsAsFactors = FALSE
  )
  mic_data <- data.frame(
    mic_id = c("NCNX06a", "NCNX06b"),
    lat = c(18.1, 18.2),
    lng = c(104.5, 104.6),
    stringsAsFactors = FALSE
  )
  state <- data.frame(rec_id = "b1", suspect_bearing = TRUE, stringsAsFactors = FALSE)

  arrows <- prepare_bearing_arrows(rows, mic_data, state, distance_m = 500)

  expect_equal(arrows$rec_id, c("a1", "b1"))
  expect_equal(arrows$color, c("red", "grey"))
  expect_true(all(is.finite(arrows$arrow_lat)))
  expect_true(all(is.finite(arrows$arrow_lng)))
})

test_that("point status distinguishes active, grouped, and removed detections", {
  grouped <- data.frame(group_ID = c(1L, 1L), rec_id = c("a", "b"), Notes = "", stringsAsFactors = FALSE)
  removed <- data.frame(rec_id = "b", Notes = "", stringsAsFactors = FALSE)

  expect_equal(
    detection_point_status(c("a", "b", "c"), grouped, removed),
    c("grouped", "removed", "active")
  )
})

test_that("selected group ids are returned in numeric order", {
  grouped <- data.frame(group_ID = c(3L, 1L, 3L), rec_id = c("a", "b", "c"), Notes = "", stringsAsFactors = FALSE)

  expect_equal(selected_group_ids(c("c", "b"), grouped), c(1L, 3L))
})

test_that("session group order uses only detections in the current session", {
  rows <- data.frame(
    rec_id = c("s1_a", "s1_b", "s1_c"),
    toa = as.POSIXct(
      c("2026-01-01 22:00:05", "2026-01-01 22:00:15", "2026-01-01 22:00:10"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
  grouped <- data.frame(
    group_ID = c(3L, 1L, 2L, 99L),
    rec_id = c("s1_a", "s1_b", "s1_c", "other_session"),
    Notes = "",
    stringsAsFactors = FALSE
  )

  expect_equal(session_group_ids(rows, grouped), c(3L, 2L, 1L))
})

test_that("session group navigation wraps within the current session", {
  rows <- data.frame(
    rec_id = c("a", "b", "c"),
    toa = as.POSIXct(
      c("2026-01-01 22:00:00", "2026-01-01 22:00:10", "2026-01-01 22:00:20"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
  grouped <- data.frame(
    group_ID = c(10L, 20L, 30L, 40L),
    rec_id = c("a", "b", "c", "other_session"),
    Notes = "",
    stringsAsFactors = FALSE
  )

  expect_equal(next_session_group_id(NULL, 1L, rows, grouped), 10L)
  expect_equal(next_session_group_id(10L, -1L, rows, grouped), 30L)
  expect_equal(next_session_group_id(30L, 1L, rows, grouped), 10L)
  expect_equal(next_session_group_id(40L, 1L, rows, grouped), 10L)
  expect_equal(next_session_group_id(NULL, 1L, rows[FALSE, , drop = FALSE], grouped), integer(0))
})

test_that("session group display adds group ids and hover labels", {
  rows <- data.frame(
    rec_id = c("a", "b", "c", "d"),
    toa = as.POSIXct(
      c("2026-01-01 22:00:00", "2026-01-01 22:00:05", "2026-01-01 22:00:10", "2026-01-01 22:00:15"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
  grouped <- data.frame(
    group_ID = c(1L, 2L, 3L),
    rec_id = c("a", "b", "c"),
    Notes = "",
    stringsAsFactors = FALSE
  )

  displayed <- add_session_group_display(rows, grouped)

  expect_equal(displayed$group_ID, c(1L, 2L, 3L, NA_integer_))
  expect_false("group_color" %in% names(displayed))
  expect_equal(displayed$group_label[1:3], c("Group 1", "Group 2", "Group 3"))
  expect_equal(displayed$group_label[4], "")
})

test_that("group review range pads and clips to session bounds", {
  session_row <- data.frame(
    real_start = as.POSIXct("2026-01-01 22:00:00", tz = "UTC"),
    real_stop = as.POSIXct("2026-01-02 00:00:00", tz = "UTC")
  )
  rows <- data.frame(
    toa = as.POSIXct(c("2026-01-01 22:00:05", "2026-01-01 22:00:10"), tz = "UTC")
  )

  range <- group_review_range(rows, session_row)

  expect_equal(range[[1]], session_row$real_start[[1]])
  expect_equal(range[[2]], as.POSIXct("2026-01-01 22:00:22.5", tz = "UTC"))
})

test_that("click selection toggles detection ids", {
  selected <- character(0)

  selected <- toggle_rec_id_selection(selected, "a")
  selected <- toggle_rec_id_selection(selected, "b")
  selected <- toggle_rec_id_selection(selected, "a")

  expect_equal(selected, "b")
  expect_equal(toggle_rec_id_selection(selected, NA_character_), "b")
})
