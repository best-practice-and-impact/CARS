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

languages <- c(
  "C++ / C#",
  "Java / Scala",
  "Javascript / Typescript",
  "Matlab",
  "Python",
  "R",
  "SAS",
  "SPSS",
  "SQL",
  "Stata",
  "VBA"
)

dummy_data <- derive_language_status(dummy_data)

dummy_output <- summarise_language_status(dummy_data)

test_that("output is a dataframe", {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("output does not contain missing values", {
  expect_false(any(is.na.data.frame(dummy_output)))
})

test_that("column names are correct", {
  expect_equal(colnames(dummy_output), c("name", "value", "n"))
})

test_that("programming language names are correct", {
  expect_equal(unique(dummy_output[[1]]), languages)
})

test_that("programming language status' are correct", {
  expect_equal(unique(dummy_output[[2]]),
               factor(c("both",
                        "access",
                        "knowledge",
                        "neither"),
                      levels=c("both",
                               "access",
                               "knowledge",
                               "neither")))
})

test_that("cell values are correct", {
  expect_equal(dummy_output[dummy_output$value == "both",]$n, c(0, 0, 1, 1, 0, 0, 2, 0, 0, 0, 0))
  expect_equal(dummy_output[dummy_output$value == "access",]$n, c(1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 2))
  expect_equal(dummy_output[dummy_output$value == "knowledge",]$n, c(1, 1, 0, 0, 0, 2, 0, 0, 2, 0, 0))
  expect_equal(dummy_output[dummy_output$value == "neither",]$n, c(0, 1, 1, 1, 2, 0, 0, 1, 0, 1, 0))
})
