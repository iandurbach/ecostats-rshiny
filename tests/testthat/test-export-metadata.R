export_metadata_fixture <- function() {
  timeline <- data.frame(
    rec_id = c("NCNX01d_1", "NCNX01a_1", "NCNX02a_1", "NCNX01d_2", "NCNX01a_2"),
    mic_id = c("NCNX01d", "NCNX01a", "NCNX02a", "NCNX01d", "NCNX01a"),
    detection_id = c(1, 1, 1, 2, 2),
    toa = as.POSIXct("2026-01-01", tz = "UTC") + c(5, 6, 7, 8, 9),
    Duration = 3,
    bearing = c(270, 90, NA, 180, 0),
    call_type = c("Female", "Female", "FP", NA, "Male"),
    suspect_bearing = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    session_id = c("s1", "s1", "s2", "s1", "s1")
  )
  mics <- data.frame(
    mic_id = c("NCNX02a", "NCNX01d", "NCNX01a", "NCNX01b", "NCNX01c"),
    lat = c(18.1, 18.2, 18.3, 18.4, 18.5),
    lng = c(104.5, 104.6, 104.7, 104.8, 104.9)
  )
  sessions <- data.frame(session_id = c("s1", "s2"), cluster_id = c("01", "02"),
                         real_start = as.POSIXct("2026-01-01", tz = "UTC"),
                         duration_seconds = c(60, 120))
  membership <- format_group_membership_rows(timeline[1:2, ], group_id = 314)
  list(timeline = timeline, mics = mics, sessions = sessions, membership = membership)
}

metadata_bundle <- function(f) {
  build_acre_export_bundle(f$timeline, f$mics, f$sessions, f$membership, empty_removed_membership())
}

test_that("action exports retain measurements and load older checkpoints", {
  f <- export_metadata_fixture()
  groups <- export_grouped_detections(f$membership, f$timeline)
  removed_points <- export_removed_detections(format_removed_membership_rows(f$timeline[3:5, ]), f$timeline)
  expect_equal(groups$bearing, c(270, 90))
  expect_equal(groups$call_type, c("Female", "Female"))
  expect_equal(removed_points$call_type, c("FP", NA, "Male"))
  expect_equal(removed_points$bearing, c(NA, 180, 0))
  expect_identical(export_grouped_detections(empty_group_membership(), f$timeline), empty_grouped_detections())
  expect_identical(export_removed_detections(empty_removed_membership(), f$timeline), empty_removed_detections())
  old <- groups[, setdiff(names(groups), c("bearing", "call_type"))]
  expect_equal(export_grouped_detections(import_group_membership(old, f$timeline), f$timeline), groups)
  old_removed <- removed_points[, setdiff(names(removed_points), c("bearing", "call_type"))]
  expect_equal(export_removed_detections(import_removed_membership(old_removed, f$timeline), f$timeline), removed_points)
  for (pair in list(list(groups, empty_grouped_detections()), list(removed_points, empty_removed_detections()))) {
    expect_identical(names(pair[[1]]), names(pair[[2]]))
    expect_type(pair[[2]]$call_type, "character")
    expect_type(pair[[2]]$bearing, "double")
  }
  file <- tempfile(fileext = ".RData")
  on.exit(unlink(file))
  save(groups, removed_points, file = file)
  env <- new.env()
  load(file, env)
  expect_identical(env$groups, groups)
  expect_identical(env$removed_points, removed_points)
})

test_that("acre exports preserve row identity through sorting and ID renumbering", {
  f <- export_metadata_fixture()
  bundle <- metadata_bundle(f)
  cap <- bundle$captures
  idx <- match(cap$rec_id, f$timeline$rec_id)
  expect_equal(cap$call_type, f$timeline$call_type[idx])
  expect_equal(cap$suspect_bearing, f$timeline$suspect_bearing[idx])
  expect_equal(cap$bearing, f$timeline$bearing[idx] * pi / 180)
  expect_equal(cap$recorder_ID, f$timeline$mic_id[idx])
  expect_equal(vapply(seq_len(nrow(cap)), function(i) rownames(bundle$traps[[cap$session[i]]])[cap$trap[i]], character(1)), cap$recorder_ID)
  expect_equal(cap$trap[cap$rec_id == "NCNX01d_1"], 4L)
  expect_equal(cap$group_ID[cap$ID == 1], c(314L, 314L))
  expect_true(all(is.na(cap$group_ID[cap$ID != 1])))
  expect_equal(length(unique(cap$ID)), 4L)
  expect_equal(bundle$metadata$capture_lookup$group_ID, cap$group_ID)
  f$mics <- f$mics[nrow(f$mics):1, ]
  f$mics$lat <- rev(f$mics$lat) # Geography must not determine recorder numbering.
  expect_equal(metadata_bundle(f)$captures$trap, cap$trap)
  file <- tempfile(fileext = ".RData")
  on.exit(unlink(file))
  save_acre_export_bundle(bundle, file)
  env <- new.env()
  load(file, env)
  for (name in names(bundle)) expect_identical(env[[name]], bundle[[name]])
})

# Evaluate the actual generated preparation code, without package loading or fitting.
prepare_generated_acre_script <- function(bundle, include = NULL, exclude = "FP", bearings = TRUE) {
  file <- tempfile(fileext = ".RData")
  on.exit(unlink(file))
  save_acre_export_bundle(bundle, file)
  expressions <- parse(text = acre_script_text(file))
  env <- new.env()
  for (expr in expressions) {
    if (identical(expr[[1]], as.name("library"))) next
    if (identical(expr[[1]], as.name("<-"))) {
      target <- as.character(expr[[2]])
      if (target == "dat") break
      if (target == "include_call_types" && !is.null(include)) { env$include_call_types <- include; next }
      if (target == "exclude_call_types") { env$exclude_call_types <- exclude; next }
      if (target == "use_bearings") { env$use_bearings <- bearings; next }
    }
    eval(expr, env)
  }
  env
}

test_that("generated script filters explicitly and retains effort and missing labels", {
  bundle <- metadata_bundle(export_metadata_fixture())
  env <- prepare_generated_acre_script(bundle)
  expect_equal(nrow(env$analysis_captures), 4)
  expect_named(env$analysis_captures, c("session", "ID", "trap", "toa", "bearing"))
  env <- prepare_generated_acre_script(bundle, exclude = "FP", bearings = FALSE)
  expect_equal(nrow(env$analysis_captures), 4)
  expect_false("bearing" %in% names(env$analysis_captures))
  expect_equal(env$survey.length, c(60, 120))
  expect_length(env$traps, 2)
  expect_equal(unique(env$analysis_captures$session), 1L)
  env <- prepare_generated_acre_script(bundle, include = "Female")
  expect_equal(nrow(env$analysis_captures), 2) # Explicit inclusion excludes missing labels.
  expect_equal(nrow(prepare_generated_acre_script(bundle, exclude = c("FP", "Male", "Female", NA))$analysis_captures), 0)
})

test_that("acre accepts exported trap gaps and preserves filtered empty sessions", {
  skip_if_not_installed("acre")
  bundle <- metadata_bundle(export_metadata_fixture())
  env <- prepare_generated_acre_script(bundle, exclude = "FP", bearings = FALSE)
  cap <- acre::create.capt(env$analysis_captures, traps = env$traps)
  expect_length(cap, 2)
  expect_equal(nrow(cap[[2]]$bincapt), 0)
  expect_equal(ncol(cap[[1]]$bincapt), 4)
  expect_equal(as.numeric(cap[[1]]$bincapt[1, ]), c(1, 0, 0, 1))
})


test_that("sparse app group IDs remain traceable and do not collide with singletons", {
  f <- export_metadata_fixture()
  f$membership <- rbind(f$membership, format_group_membership_rows(f$timeline[5, ], group_id = 400))
  cap <- metadata_bundle(f)$captures
  expect_equal(cap$ID[cap$group_ID %in% 314], c(1L, 1L))
  expect_equal(cap$ID[cap$group_ID %in% 400], 2L)
  expect_equal(sort(cap$ID[is.na(cap$group_ID)]), c(3L, 4L))
})

test_that("array sessions pool windows and retain zero-call arrays and effort", {
  f <- export_metadata_fixture()
  extra <- f$sessions[c(1, 1), ]
  extra$session_id <- c("s3", "s4")
  extra$cluster_id <- c("01", "03")
  extra$real_start <- extra$real_start + 86400
  extra$duration_seconds <- c(180, 60)
  f$sessions <- rbind(f$sessions, extra)
  f$mics <- rbind(f$mics, data.frame(mic_id = "NCNX03a", lat = 18.2, lng = 104.6))
  bundle <- metadata_bundle(f)
  expect_equal(bundle$sessions$cluster_id, c("01", "02", "03"))
  expect_equal(bundle$survey.length, c(240, 120, 60))
  expect_equal(length(bundle$traps), 3)
  expect_equal(nrow(bundle$captures), nrow(f$timeline))
  expect_equal(bundle$metadata$recording_windows$session_id, c("s1", "s2", "s3", "s4"))
  expect_equal(rownames(bundle$traps[[3]]), "NCNX03a")
  expect_match(acre_script_text(), "# If incorrect add session-specific survey lengths manually", fixed = TRUE)
  skip_if_not_installed("acre")
  capt <- bundle$captures[, c("session", "ID", "trap", "toa")]
  dat <- acre::read.acre(captures = capt, traps = bundle$traps,
                         survey.length = bundle$survey.length,
                         control.mask = list(buffer = 1000))
  expect_length(dat$capt, 3)
  expect_equal(nrow(dat$capt[[3]]$bincapt), 0)
})

test_that("pooling rejects groups spanning distinct recording windows", {
  f <- export_metadata_fixture()
  extra <- f$sessions[1, ]
  extra$session_id <- "s3"
  f$sessions <- rbind(f$sessions, extra)
  f$timeline$session_id[2] <- "s3"
  expect_error(metadata_bundle(f), "group spans multiple recording windows")
})


test_that("generated acre script matches the reviewed example", {
  example <- test_path("..", "..", "..", "acre", "example-fit-script.R")
  skip_if_not(file.exists(example))
  expected <- readLines(example)
  generated <- strsplit(acre_script_text("acre_inputs_20261104_150529.RData"), "\n", fixed = TRUE)[[1]]
  expect_identical(generated, expected)
})
