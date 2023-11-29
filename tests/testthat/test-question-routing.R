test_that("check_skip_logic returns failing rows from one skipped column", {
  dummy_data <- data.frame(cond_col = c(F, F, T, T), skipped_col = c("test", NA, "test", NA))

  got <- check_skip_logic(dummy_data, dummy_data$cond, "skipped_col")

  expected <- 3

  expect_equal(expected, got)
})

test_that("check_skip_logic returns failing rows from multiple skipped columns", {
  dummy_data <- data.frame(cond_col = c(F, F, F, T, T, T),
                           skipped_col1 = c(NA, NA, NA, "test", NA, NA),
                           skipped_col2 = c(NA, NA, NA, NA, "test", NA),
                           skipped_col3 = c(NA, NA, NA, NA, NA, "test"))

  got <- check_skip_logic(dummy_data, dummy_data$cond, c("skipped_col1", "skipped_col2", "skipped_col3"))

  expected <- c(4, 5, 6)

  expect_equal(expected, got)
})


test_that("enforce_skip_logic replaces failing rows from one skipped column", {
  dummy_data <- data.frame(cond_col = c(F, F, T, T),
                           skipped_col = c("test", NA, "test", NA))

  got <- enforce_skip_logic(dummy_data, dummy_data$cond, "skipped_col")

  expected <- data.frame(cond_col = c(F, F, T, T),
                         skipped_col = c("test", NA, NA, NA))

  expect_equal(expected, got)
})

test_that("enforce_skip_logic replaces failing rows from multiple skipped columns", {
  dummy_data <- data.frame(cond_col = c(F, F, F, T, T, T),
                           skipped_col1 = c(NA, "test", "test", NA, "test", "test"),
                           skipped_col2 = c(NA, NA, "test", NA, NA, "test"),
                           skipped_col3 = c(NA, NA, NA, NA, NA, NA))

  got <- enforce_skip_logic(dummy_data, dummy_data$cond, c("skipped_col1", "skipped_col2", "skipped_col3"))

  expected <- data.frame(cond_col = c(F, F, F, T, T, T),
                         skipped_col1 = c(NA, "test", "test", NA, NA, NA),
                         skipped_col2 = c(NA, NA, "test", NA, NA, NA),
                         skipped_col3 = c(NA, NA, NA, NA, NA, NA))

  expect_equal(expected, got)
})

test_that("apply_skip_logic replaces all relevant columns with NAs", {
  dummy_data <- rename_cols(cars_dummy_data)

  got <- apply_skip_logic(dummy_data)

  expected <- rename_cols(cars_dummy_data_clean)

  expect_equal(expected, got)
})
