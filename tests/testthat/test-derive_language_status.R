
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
  knowledge_JS = c("Don't know", "Yes"),
  access_JS = c("No", "Yes"),
  knowledge_java = c("Don't know", "Yes"),
  access_java = c("Don't know", "No"),
  knowledge_C = c("Yes", "Don't know"),
  access_C = c("No", "Yes"),
  knowledge_matlab = c("Yes", "No"),
  access_matlab = c("Yes", "No")
)

test_that("derive_language_status output is as expected", {

  got <- derive_language_status(dummy_data)

  expected <- data.frame(
    dummy_data,
    status_R = c("knowledge", "knowledge"),
    status_SQL = c("knowledge", "knowledge"),
    status_SAS = c("both", "both"),
    status_VBA = c("access", "access"),
    status_python = c("neither", "neither"),
    status_SPSS = c("neither", "access"),
    status_stata = c("access", "neither"),
    status_JS = c("neither", "both"),
    status_java = c("neither", "knowledge"),
    status_C = c("knowledge", "access"),
    status_matlab = c("both", "neither")
  )

  expect_equal(got, expected)

})
