dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 4), rep("All the time", 6)),
                         basic_rap_score = c(0, 1, 2, 2, 3, 3, 3, 4, 4, 5, 6))

dummy_output <- summarise_rap_basic(dummy_data)

test_that("Check output is dataframe" , {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output does not contain missing values", {
  expect_false(any(is.na(dummy_output)))
})

test_that("Check number of rows in output", {
  expect_equal(nrow(dummy_output), 7)
})

test_that("Check number of columns in output", {
  expect_equal(ncol(dummy_output), 2)
})

test_that("Output column names are correct", {
  expect_equal(colnames(dummy_output), c("value", "n"))
})


test_that("Values in output are correct", {
  expect_equal(dummy_output[[2]], c(0, 1, 2, 3, 2, 1, 1))
})
