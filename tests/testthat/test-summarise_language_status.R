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
  knowledge_java = c("Don't know", "Yes"),
  access_java = c("Don't know", "No"),
  knowledge_C = c("Yes", "Don't know"),
  access_C = c("No", "Yes"),
  knowledge_matlab = c("Yes", "No"),
  access_matlab = c("Yes", "No")
)

dummy_data <- derive_language_status(dummy_data) # Will never return NAs

test_that("summarise_language_status output is as expected", {

  got <- summarise_language_status(dummy_data)

  expected <- data.frame(name = c(rep("C++ / C#", 4),
                                  rep("Java / Scala", 4),
                                  rep("Javascript / Typescript", 4),
                                  rep("Matlab", 4),
                                  rep("Python", 4),
                                  rep("R", 4),
                                  rep("SAS", 4),
                                  rep("SPSS", 4),
                                  rep("SQL", 4),
                                  rep("Stata", 4),
                                  rep("VBA", 4)),
                         value = factor(rep(c("both",
                                              "access",
                                              "knowledge",
                                              "neither"),
                                            11),
                                        levels=c("both",
                                                 "access",
                                                 "knowledge",
                                                 "neither")),
                         n = c(0.00, 0.50, 0.50, 0.00, 0.00, 0.00, 0.50, 0.50, 0.50, 0.00, 0.00,
                               0.50, 0.50, 0.00, 0.00, 0.50, 0.00, 0.00, 0.00, 1.00, 0.00, 0.00,
                               1.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00, 0.50, 0.00, 0.50, 0.00,
                               0.00, 1.00, 0.00, 0.00, 0.50, 0.00, 0.50, 0.00, 1.00, 0.00, 0.00))

  expect_equal(got, expected)

})
