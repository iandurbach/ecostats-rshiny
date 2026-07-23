find_file_upwards <- function(filename, start = getwd()) {
  current <- normalizePath(start, mustWork = TRUE)
  repeat {
    candidate <- file.path(current, filename)
    if (file.exists(candidate)) return(candidate)
    parent <- dirname(current)
    if (identical(parent, current)) return(NA_character_)
    current <- parent
  }
}

clusterer_script <- find_file_upwards(file.path("ecostats-rshiny", "group_gibbon_detections.R"))
if (is.na(clusterer_script)) {
  clusterer_script <- find_file_upwards("group_gibbon_detections.R")
}
if (!is.na(clusterer_script)) {
  source(clusterer_script)
}

skip_if_no_clusterer_script <- function() {
  skip_if(is.na(clusterer_script), "Standalone clusterer script is outside the package test copy.")
}

make_clusterer_recordings <- function() {
  data.frame(
    recording_ID = c("a1", "b1", "a2", "c1", "b2", "c2"),
    mic_ID = c("A", "B", "A", "C", "B", "C"),
    GPSDatetime2 = c(
      "2026-01-01 00:00:00.100",
      "2026-01-01 00:00:01.100",
      "2026-01-01 00:00:02.100",
      "2026-01-01 00:00:20.100",
      "2026-01-01 00:00:21.100",
      "2026-01-01 00:00:40.100"
    ),
    measured_bearing = c(90, 270, 110, 180, 10, 45),
    measured_gender = NA_character_,
    spectrogram = paste0("s", 1:6, ".png"),
    Duration = rep(2, 6),
    stringsAsFactors = FALSE
  )
}

test_that("candidate generation excludes singletons and duplicate-recorder groups", {
  skip_if_no_clusterer_script()
  prepared <- prepare_grouping_inputs(make_clusterer_recordings())
  candidates <- generate_candidate_groups(prepared$detections, max_group_span_seconds = 4)

  expect_gt(nrow(candidates), 0)
  expect_true(all(candidates$group_size >= 2))
  members <- candidate_members(candidates)
  for (ids in members) {
    rows <- prepared$detections[match(ids, prepared$detections$rec_id), , drop = FALSE]
    expect_equal(anyDuplicated(rows$mic_id), 0L)
  }
})

test_that("set packing assigns each selected detection at most once", {
  skip_if_no_clusterer_script()
  result <- group_gibbon_detections(make_clusterer_recordings(), max_group_span_seconds = 4, solver = "lpsolve")

  expect_gt(nrow(result$group_membership), 0)
  expect_equal(anyDuplicated(result$group_membership$rec_id), 0L)
  expect_named(
    result$grouped_detections,
    c("group_ID", "detection_ID", "recorder_ID", "detection_start_time", "detection_end_time", "Notes", "suspect_bearing")
  )
})

test_that("saved clusterer output matches app grouping RData format", {
  skip_if_no_clusterer_script()
  recordings <- make_clusterer_recordings()
  result <- group_gibbon_detections(recordings, max_group_span_seconds = 4, solver = "lpsolve")
  out_path <- tempfile(fileext = ".RData")

  save_app_loadable_groups(result, out_path)

  env <- new.env(parent = emptyenv())
  load(out_path, envir = env)
  expect_true(exists("groups", envir = env, inherits = FALSE))
  expect_true(exists("removed_points", envir = env, inherits = FALSE))
  expect_false(exists("result", envir = env, inherits = FALSE))
  expect_named(
    env$groups,
    c("group_ID", "detection_ID", "recorder_ID", "detection_start_time", "detection_end_time", "Notes", "suspect_bearing")
  )
  expect_named(
    env$removed_points,
    c("detection_ID", "recorder_ID", "detection_start_time", "detection_end_time", "Notes", "suspect_bearing")
  )

  timeline <- prepare_grouping_inputs(recordings)$detections
  imported <- import_group_membership(env$groups, timeline)
  expect_equal(imported$rec_id, result$group_membership$rec_id)
})

test_that("ungrouped detections remain available", {
  skip_if_no_clusterer_script()
  result <- group_gibbon_detections(make_clusterer_recordings(), max_group_span_seconds = 4, solver = "lpsolve")

  expect_true("c2" %in% result$ungrouped_detections$rec_id)
  expect_false(any(result$ungrouped_detections$rec_id %in% result$group_membership$rec_id))
})

test_that("bearing support is weak evidence and not a hard filter", {
  skip_if_no_clusterer_script()
  recordings <- make_clusterer_recordings()[1:2, , drop = FALSE]
  mics <- data.frame(
    mic_id = c("A", "B"),
    lat = c(0, 0),
    lng = c(0, 0.001),
    stringsAsFactors = FALSE
  )
  prepared <- prepare_grouping_inputs(recordings, mics)
  candidates <- generate_candidate_groups(
    prepared$detections,
    prepared$mics,
    max_group_span_seconds = 4,
    bearing_weight = 0.2,
    min_score = -Inf
  )

  expect_equal(nrow(candidates), 1L)
  expect_true(is.finite(candidates$bearing_score))
  expect_gt(candidates$score, -Inf)
})

test_that("temporal scoring responds to every candidate member", {
  skip_if_no_clusterer_script()
  base_time <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  compact <- data.frame(
    rec_id = paste0("compact", 1:5),
    mic_id = LETTERS[1:5],
    toa = base_time + c(0, 0.1, 0.2, 0.3, 0.4)
  )
  with_outlier <- compact
  with_outlier$toa[[5]] <- base_time + 8

  compact_score <- score_candidate_group(compact)
  outlier_score <- score_candidate_group(with_outlier)

  expect_lt(outlier_score$temporal_compactness, 0.25)
  expect_lt(outlier_score$score, 0)
  expect_gt(compact_score$score, outlier_score$score)
})

test_that("large incoherent candidates are filtered instead of rewarded by size", {
  skip_if_no_clusterer_script()
  base_time <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  detections <- data.frame(
    rec_id = paste0("d", 1:5),
    mic_id = LETTERS[1:5],
    toa = base_time + c(0, 0, 0, 0, 8)
  )

  candidates <- generate_candidate_groups(detections, min_score = 0.1)

  expect_false(any(candidates$group_size == 5))
})

test_that("invalid scoring scales fail clearly", {
  skip_if_no_clusterer_script()
  prepared <- prepare_grouping_inputs(make_clusterer_recordings()[1:2, ])

  expect_error(
    generate_candidate_groups(prepared$detections, time_sigma_seconds = 0),
    "greater than zero"
  )
  expect_error(
    generate_candidate_groups(prepared$detections, chance_compactness = 2),
    "between zero and one"
  )
})

test_that("bearing score uses a fitted common source and reports its residual", {
  skip_if_no_clusterer_script()
  rows <- data.frame(
    rec_id = c("a", "b", "c"),
    mic_id = c("A", "B", "C"),
    toa = as.POSIXct("2026-01-01", tz = "UTC"),
    bearing = c(45, 315, 180)
  )
  mics <- data.frame(
    mic_id = c("A", "B", "C"),
    lat = c(0, 0, 0.001),
    lng = c(0, 0.001, 0.0005)
  )

  bearing <- candidate_bearing_score(rows, mics)

  expect_true(is.finite(bearing$miss_degrees))
  expect_gt(bearing$score, 0)
})

test_that("sample SQLite subset can be grouped", {
  skip_if_no_clusterer_script()
  db_paths <- test_path("..", "..", "..", "data", "db", "NCNX06a_database.sqlite3")
  skip_if_not(file.exists(db_paths))

  input <- read_grouping_databases(db_paths)
  result <- group_gibbon_detections(input$recData, input$micData, max_group_span_seconds = 8, solver = "lpsolve")

  expect_named(result, c(
    "group_membership", "grouped_detections", "candidates", "selected_candidate_ids",
    "ungrouped_detections", "parameters"
  ))
  expect_true(all(c("candidate_id", "rec_ids", "score") %in% names(result$candidates)))
})
