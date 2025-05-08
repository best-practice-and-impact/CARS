
# Data operations table
# Input data should never include missing data.
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
  knowledge_matlab = c("Yes", "No"),
  access_matlab = c("Yes", "No")
)

test_that("derive_language_status output is as expected", {

  got <- derive_language_status(dummy_data)

  expected <- data.frame(
    dummy_data,
    status_R = c("Knowledge Only", "Knowledge Only"),
    status_SQL = c("Knowledge Only", "Knowledge Only"),
    status_SAS = c("Both", "Both"),
    status_VBA = c("Access Only", "Access Only"),
    status_python = c("Neither", "Neither"),
    status_SPSS = c("Neither", "Access Only"),
    status_stata = c("Access Only", "Neither"),
    status_matlab = c("Both", "Neither")
  )

  expect_equal(got, expected)

})
