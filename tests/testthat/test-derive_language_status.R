# Data operations table
# Frequency table should not include missing values and should include counts of 0

dummy_data <- data.frame(
  knowledge_R = c("Yes", "Yes"),
  access_R = c("No", "No"),
  knowledge_SQL = c("Yes", "Yes"),
  access_SQL = c("Don't know", "No"),
  knowledge_SAS = c("Yes", "Yes"),
  access_SAS = c("Yes", "Yes"),
  knowledge_VBA = c("No", "No"),
  access_VBA = c("Yes", "Yes"),
  knowledge_python = c("No", "No"),
  access_python = c("Don't know", "Don't know"),
  knowledge_SPSS = c("No", "No"),
  access_SPSS = c("No", "Yes"),
  knowledge_stata = c("Don't know", "No"),
  access_stata = c("Yes", "No"),
  knowledge_JS = c("Don't know", "Yes"),
  access_JS = c("No", "Yes"),
  knowledge_java_scala = c("Don't know", "Yes"),
  access_java_scala = c("Don't know", "No"),
  knowledge_C = c("Yes", "Don't know"),
  access_C = c("No", "Yes"),
  knowledge_matlab = c("Yes", "No"),
  access_matlab = c("Yes", "No")
)

dummy_output <- derive_language_status(dummy_data)

test_that("output is a dataframe", {
  expect_s3_class(dummy_output, "data.frame")
})

new_cols <- c("status_R",
              "status_SQL",
              "status_SAS",
              "status_VBA",
              "status_python",
              "status_SPSS",
              "status_stata",
              "status_JS",
              "status_java_scala",
              "status_C",
              "status_matlab")

test_that("output has new columns", {
  expect_equal(new_cols, colnames(dummy_output)[23:33])
})

test_that("output does not contain missing values", {
  expect_false(any(is.na(dummy_output)))
})

test_that("Check number of rows in output", {
  expect_equal(nrow(dummy_output), 2)
})

test_that("Check number of columns in output", {
  expect_equal(ncol(dummy_output), 33)
})

expected_output <- data.frame(
  dummy_data,
  status_R = c("knowledge", "knowledge"),
  status_SQL = c("knowledge", "knowledge"),
  status_SAS = c("both", "both"),
  status_VBA = c("access", "access"),
  status_python = c("neither", "neither"),
  status_SPSS = c("neither", "access"),
  status_stata = c("access", "neither"),
  status_JS = c("neither", "both"),
  status_java_scala = c("neither", "knowledge"),
  status_C = c("knowledge", "access"),
  status_matlab = c("both", "neither")
)

test_that("output matches expected output", {
  expect_equal(dummy_output, expected_output)
})
