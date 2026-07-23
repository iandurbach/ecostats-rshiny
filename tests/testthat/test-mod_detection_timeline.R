testServer(
  mod_detection_timeline_server,
  args = list(),
  {
    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    session_summary <- suppressWarnings(output$session_summary)
    input_status <- suppressWarnings(output$input_status)
    export_status <- suppressWarnings(output$export_status)
    expect_equal(session_summary, "No data loaded")
    expect_match(input_status, "0 database\\(s\\) selected")
    expect_match(export_status, "0 grouped rows")
  }
)

test_that("detection timeline module UI exposes the new workflow controls", {
  ui <- mod_detection_timeline_ui(id = "test")
  html <- paste(as.character(ui), collapse = "\n")

  golem::expect_shinytaglist(ui)
  expect_match(html, "Select SQLite database\\(s\\)")
  expect_match(html, "Select WAV root folder")
  expect_match(html, "Load existing groups")
  expect_match(html, "Export RData")
  expect_match(html, "Export acre inputs")
  expect_match(html, "Export acre script")
  expect_match(html, "timeline_plot")
  expect_match(html, "selection_map")
  expect_match(html, "selected_spectrograms")

  fmls <- formals(mod_detection_timeline_ui)
  expect_true("id" %in% names(fmls))
})
