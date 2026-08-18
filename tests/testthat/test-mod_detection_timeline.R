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

  expect_s3_class(ui, "shiny.tag.list")
  expect_match(html, "Select SQLite database\\(s\\)")
  expect_match(html, "Select WAV root folder")
  expect_match(html, "Load existing groups")
  expect_match(html, "Export RData")
  expect_match(html, "Export acre inputs")
  expect_match(html, "Export acre script")
  expect_match(html, "timeline_plot")
  expect_match(html, "selection_map")
  expect_match(html, "selected_spectrograms")
  expect_match(html, "Previous cluster")
  expect_match(html, "Next cluster")
  expect_match(html, 'btn-sm" id="test-prev_cluster"')
  expect_match(html, 'btn-sm" id="test-next_cluster"')
  expect_match(html, 'btn-sm" id="test-prev_session"')
  expect_match(html, 'btn-sm" id="test-next_session"')
  expect_match(html, "Auto group")
  expect_match(html, "Auto group all")
  expect_match(html, "Clear all groups")
  expect_lt(regexpr("action_notes", html)[[1]], regexpr("group_selected", html)[[1]])
  expect_lt(regexpr("next_group", html)[[1]], regexpr("auto_group", html)[[1]])

  fmls <- formals(mod_detection_timeline_ui)
  expect_true("id" %in% names(fmls))
})

test_that("server-created confirmation controls use the module session namespace", {
  server_code <- paste(deparse(body(mod_detection_timeline_server)), collapse = "\n")

  expect_match(server_code, 'session\\$ns\\("confirm_clear_session_groups"\\)')
  expect_false(grepl('actionButton\\(ns\\("confirm_clear_session_groups"', server_code))
})
