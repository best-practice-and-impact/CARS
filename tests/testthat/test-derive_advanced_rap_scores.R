dummy_data <- data.frame(prac_functions = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"),
                         prac_unit_test = c("Never", "All the time", "Regularly", "Sometimes", "Rarely"),
                         doc_functions = c("Rarely", "Never", "All the time", "Regularly", "Sometimes"),
                         prac_package = c("Sometimes", "Rarely", "Never", "All the time", "Regularly"),
                         prac_style = c("Regularly", "Sometimes", "Rarely", "Never", "All the time"),
                         prac_automated_QA = c("Yes", "No", "Yes", "No", "I don't know what continuous integration is"),
                         prac_dir_structure = c("I don't know what continuous integration is", "Yes", "No", "Yes", "No"))

dummy_output <- derive_advanced_rap_scores(dummy_data)

test_that("output is a dataframe", {
  expect_s3_class(dummy_output, "data.frame")
})

new_cols <- c("function_score",
              "unit_test_score",
              "function_doc_score",
              "package_score",
              "code_style_score",
              "cont_integreation_score",
              "dep_management_score",
              "advanced_rap_score")

test_that("output has new columns", {
  expect_true(identical(new_cols, colnames(dummy_output[8:15])))
})

test_that("output does not contain missing values", {
  expect_false(any(is.na(dummy_output)))
})

test_that("Check number of rows in output", {
  expect_equal(nrow(dummy_output), 5)
})

test_that("Check number of columns in output", {
  expect_equal(ncol(dummy_output), 15)
})

test_that("output values are correct", {
  expect_equal(dummy_output[[8]], c(1, 1, 0, 0, 0))
  expect_equal(dummy_output[[9]], c(0, 1, 1, 0, 0))
  expect_equal(dummy_output[[10]], c(0, 0, 1, 1, 0))
  expect_equal(dummy_output[[11]], c(0, 0, 0, 1, 1))
  expect_equal(dummy_output[[12]], c(1, 0, 0, 0, 1))
  expect_equal(dummy_output[[13]], c(1, 0, 1, 0, 0))
  expect_equal(dummy_output[[14]], c(0, 1, 0, 1, 0))
  expect_equal(dummy_output[[15]], c(3, 3, 3, 3, 2))
})
