
# Data operations table
# Input data should never include missing data.
dummy_data <- data.frame(
  knowledge_R = c("Yes", "Yes"),
  use_R = c("No", "No"),
  knowledge_SQL = c("Yes", "Yes"),
  use_SQL = c("Don't know", "No"),
  knowledge_SAS = c("Yes", "Yes"),
  use_SAS = c("Yes", "Yes"),
  knowledge_VBA = c("No", "No"),
  use_VBA = c("Yes", "Yes"),
  knowledge_python = c("No", "No"),
  use_python = c("Don't know", "Don't know"),
  knowledge_SPSS = c("No", "No"),
  use_SPSS = c("No", "Yes"),
  knowledge_stata = c("Don't know", "No"),
  use_stata = c("Yes", "No"),
  knowledge_matlab = c("Yes", "No"),
  use_matlab = c("Yes", "No"),
  knowledge_dax = c("Yes", "No"),
  use_dax = c("No", "No"),
  use_spark = c("Don't know", "Don't know"),
  knowledge_spark = c("Don't know", "Don't know")
)

test_that("derive_language_status output is as expected", {

  got <- derive_language_status(dummy_data)

  expected <- data.frame(
    dummy_data,
    status_R = c("Knowledge Only", "Knowledge Only"),
    status_SQL = c("Knowledge Only", "Knowledge Only"),
    status_SAS = c("Both", "Both"),
    status_VBA = c("Use Only", "Use Only"),
    status_python = c("Neither", "Neither"),
    status_SPSS = c("Neither", "Use Only"),
    status_stata = c("Use Only", "Neither"),
    status_matlab = c("Both", "Neither"),
    status_dax = c("Knowledge Only", "Neither"),
    status_spark = c("Neither", "Neither")
  )

  expect_equal(got, expected)

})
