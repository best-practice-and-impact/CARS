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
  knowledge_matlab = c("Yes", "No"),
  access_matlab = c("Yes", "No")
)

dummy_data <- derive_language_status(dummy_data) # Will never return NAs

test_that("summarise_language_status output is as expected", {

  got <- summarise_language_status(dummy_data)

  expected <- data.frame(name = rep(c("Matlab",
                                      "Python",
                                      "R",
                                      "SAS",
                                      "SPSS",
                                      "SQL",
                                      "Stata",
                                      "VBA"), each=3),
                         value = factor(rep(c("Access Only",
                                              "Both",
                                              "Knowledge Only"),
                                            8),
                                        levels=c("Access Only",
                                                 "Both",
                                                 "Knowledge Only")),
                         n = c(0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                               1.00, 0.00, 1.00, 0.00, 1.00, 0.00, 0.00, 0.00,
                               0.00, 1.00, 1.00, 0.00, 0.00, 1.00, 0.00, 0.00))

  expect_equal(got, expected)

})
