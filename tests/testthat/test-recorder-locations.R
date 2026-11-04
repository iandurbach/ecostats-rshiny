location_csv <- function(text) {
  path <- tempfile(fileext = ".csv")
  writeLines(text, path)
  path
}

test_that("verified CSVs use named columns and replace positions by recorder ID", {
  path <- location_csv(c("latitude,extra,recorder_ID,longitude", "19,x, NCNX06b ,105", "18,x,NCNX06a,104", "20,x,unused,106"))
  on.exit(unlink(path))
  locations <- read_verified_recorder_locations(path)
  mics <- data.frame(mic_id = c("NCNX06a", "NCNX06b"), lng = 1:2, lat = 3:4,
                     cluster_id = "06", recorder_heading = c(10, 20))
  updated <- apply_verified_recorder_locations(mics, locations)
  expect_equal(updated$lng, c(104, 105))
  expect_equal(updated$lat, c(18, 19))
  expect_identical(updated[c("mic_id", "cluster_id", "recorder_heading")],
                   mics[c("mic_id", "cluster_id", "recorder_heading")])
  expect_error(apply_verified_recorder_locations(mics, locations[1, ]), "NCNX06a")
})

test_that("invalid headers, IDs and coordinates are rejected", {
  cases <- list(
    c("id,longitude,latitude", "a,104,18"),
    c("recorder_ID,longitude,latitude,latitude", "a,104,18,18"),
    "recorder_ID,longitude,latitude",
    c("recorder_ID,longitude,latitude", ",104,18"),
    c("recorder_ID,longitude,latitude", "a,104,18", " a ,105,19")
  )
  for (csv in cases) {
    path <- location_csv(csv)
    expect_error(read_verified_recorder_locations(path))
    unlink(path)
  }
  for (value in c("", "NA", "NaN", "Inf", "west", "181", "-181")) {
    path <- location_csv(c("recorder_ID,longitude,latitude", paste0("a,", value, ",18")))
    expect_error(read_verified_recorder_locations(path), "Invalid longitude.*a")
    unlink(path)
  }
  for (value in c("", "NA", "Inf", "north", "91", "-91")) {
    path <- location_csv(c("recorder_ID,longitude,latitude", paste0("a,104,", value)))
    expect_error(read_verified_recorder_locations(path), "Invalid latitude.*a")
    unlink(path)
  }
  path <- location_csv(c("recorder_ID,longitude,latitude", "a,180,90", "b,-180,-90", "c,0,0"))
  expect_equal(nrow(read_verified_recorder_locations(path)), 3)
  unlink(path)
})

test_that("the full loaded recorder set is required, including silent recorders", {
  mics <- data.frame(mic_id = c("detected", "silent"), lng = 0, lat = 0)
  path <- location_csv(c("recorder_ID,longitude,latitude", "detected,104,18"))
  on.exit(unlink(path))
  expect_error(apply_verified_recorder_locations(mics, read_verified_recorder_locations(path)), "silent")
})

test_that("uploads apply atomically and feed grouping geometry and exports without losing work", {
  paths <- test_path("..", "..", "..", "data", "db", paste0("NCNX06", c("a", "b", "c"), "_database.sqlite3"))
  skip_if_not(all(file.exists(paths)))
  good <- location_csv(c("recorder_ID,longitude,latitude", "NCNX06b,105.1,19.1", "NCNX06a,105,19"))
  second <- location_csv(c("recorder_ID,longitude,latitude", "NCNX06a,105.2,19.2", "NCNX06b,105.3,19.3"))
  bad <- location_csv(c("recorder_ID,longitude,latitude", "NCNX06a,106,20"))
  malformed <- location_csv(c("recorder_ID,longitude,latitude", "NCNX06a,no,20"))
  on.exit(unlink(c(good, second, bad, malformed)))
  upload <- function(path) data.frame(name = basename(path), datapath = path, size = file.info(path)$size, type = "text/csv")
  shiny::testServer(mod_detection_timeline_server, {
    # The headless test has no browser to register Plotly's timeline events.
    session$userData$plotlyShinyEventIDs <- paste(c("plotly_relayout", "plotly_selected", "plotly_click"), session$ns("timeline"), sep = "-")
    selected_db_paths(paths[1:2])
    selected_wav_root(tempdir())
    expect_match(output$recorder_locations_input$html, "Update recorder locations")
    session$setInputs(recorder_locations = upload(good))
    expect_null(mic_data())
    expect_match(output$input_status, "CSV staged")
    session$setInputs(load_data = 1)
    expect_equal(mic_data()$lng, c(105, 105.1))
    expect_equal(location_source()$type, "csv")
    original_mics <- database_mic_data()
    membership <- format_group_membership_rows(head(timeline_data(), 1), group_id = 314)
    group_membership(membership)
    removed <- format_removed_membership_rows(tail(timeline_data(), 1), notes = "noise")
    removed_membership(removed)
    suspect_bearing_state(data.frame(rec_id = membership$rec_id, suspect_bearing = TRUE))
    session$setInputs(recorder_locations = upload(second))
    expect_equal(mic_data()$lng, c(105.2, 105.3))
    expect_identical(group_membership(), membership)
    expect_identical(removed_membership(), removed)
    expect_true(suspect_bearing_state()$suspect_bearing)
    expect_identical(current_cluster_mics()$lng, mic_data()$lng)
    arrows <- prepare_bearing_arrows(head(timeline_data(), 1), current_cluster_mics())
    expect_equal(arrows$lng, mic_data()$lng[match(arrows$mic_id, mic_data()$mic_id)])
    bundle <- acre_bundle()
    expect_equal(bundle$metadata$trap_lookup$lng, mic_data()$lng)
    expect_equal(bundle$metadata$location_source$filename, basename(second))
    previous <- mic_data()
    session$setInputs(recorder_locations = upload(bad))
    expect_identical(mic_data(), previous)
    expect_match(output$input_status, "CSV rejected:.*NCNX06b")
    expect_equal(acre_bundle()$metadata$location_source$filename, basename(second))
    session$setInputs(load_data = 2)
    expect_identical(mic_data(), previous)
    expect_identical(group_membership(), membership)
    session$setInputs(recorder_locations = upload(malformed))
    expect_identical(mic_data(), previous)
    session$setInputs(load_data = 3)
    expect_identical(mic_data(), previous)
    session$setInputs(recorder_locations = upload(second))
    selected_db_paths(paths)
    session$setInputs(load_data = 4)
    expect_identical(mic_data(), previous) # New recorder c is absent from CSV.
    expect_identical(group_membership(), membership)
    session$setInputs(reset_recorder_locations = 1)
    expect_identical(mic_data(), original_mics)
    expect_equal(location_source()$type, "database")
    expect_null(location_upload())
    expect_identical(group_membership(), membership)
    expect_identical(acre_bundle()$metadata$location_source$type, "database")
  })
})

test_that("upload placeholder is below Load data and above Call type", {
  html <- as.character(mod_detection_timeline_ui("test"))
  expect_lt(regexpr('id="test-load_data"', html)[1], regexpr('id="test-recorder_locations_input"', html)[1])
  expect_lt(regexpr('id="test-recorder_locations_input"', html)[1], regexpr('id="test-call_type_filter"', html)[1])
})
