test_that("apply_skip_logic replaces all relevant columns with NAs", {
  dummy_data <- rename_cols(cars_dummy_data_test, config)

  got <- apply_skip_logic(dummy_data)

  expected <- rename_cols(cars_dummy_data_clean, config)

  expect_equal(expected, got)
})
