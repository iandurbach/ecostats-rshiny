test_that("arrow layer ids preserve numeric and database recording ids", {
  expect_equal(rec_id_from_arrow_layer_id("arrow_12"), "12")
  expect_equal(rec_id_from_arrow_layer_id("arrow_NCNX06a_1"), "NCNX06a_1")
})

test_that("suspect bearing toggling supports character recording ids", {
  clicked_rec_id <- rec_id_from_arrow_layer_id("arrow_NCNX06a_1")
  rec_data <- data.frame(
    rec_id = c("NCNX06a_1", "NCNX06a_2"),
    suspect_bearing = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  updated <- dplyr::mutate(
    rec_data,
    suspect_bearing = dplyr::if_else(as.character(rec_id) == !!clicked_rec_id, !suspect_bearing, suspect_bearing)
  )

  expect_true(updated$suspect_bearing[[1]])
  expect_false(updated$suspect_bearing[[2]])
})
