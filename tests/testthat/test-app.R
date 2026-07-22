test_that("app UI mounts the detection timeline workspace directly", {
  ui <- app_ui(request = NULL)
  html <- paste(as.character(ui), collapse = "\n")

  expect_match(html, "detection_timeline")
  expect_match(html, "timeline_plot")
  expect_match(html, "selection_map")
  expect_false(grepl("vocostep", html, fixed = TRUE))
  expect_false(grepl("mod_wizard", html, fixed = TRUE))
})
