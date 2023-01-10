# Coding tools frequency tables (access or knowledge)

dummy_data <- data.frame(
  knowledge_R = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_R = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_SQL = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_SQL = c("Yes", rep("No", 3), rep("Don't Know", 2)),
  knowledge_SAS = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  access_SAS = c(rep("Yes", 3), "No", rep("Don't Know", 2)),
  knowledge_VBA = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_VBA = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_python = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_python = c("Yes", rep("No", 3), rep("Don't Know", 2)),
  knowledge_SPSS = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  access_SPSS = c(rep("Yes", 3), "No", rep("Don't Know", 2)),
  knowledge_stata = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_stata = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_JS = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_JS = c("Yes", rep("No", 3), rep("Don't Know", 2)),
  knowledge_java = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  access_java = c(rep("Yes", 3), "No", rep("Don't Know", 2)),
  knowledge_C = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_C = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_matlab = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_matlab = c("Yes", rep("No", 5), rep("Don't Know", 0)) # Used to check zero counts aren't missing
)

test_that("summarise_coding_tools missing data is handled correctly", {

  dummy_data_missing_values <- dummy_data
  dummy_data_missing_values[1,] <- NA

  got_missing <- summarise_coding_tools(dummy_data_missing_values, "knowledge")

  expect_false(any(is.na(got_missing)))

})

test_that("summarise_coding_tools knowledge output is as expected", {

  got_knowledge <- summarise_coding_tools(dummy_data, "knowledge")

  expected_knowledge <- data.frame("name" = c(rep("C++ / C#", 3),
                                              rep("Java / Scala", 3),
                                              rep("Javascript / Typescript", 3),
                                              rep("Matlab", 3),
                                              rep("Python", 3),
                                              rep("R", 3),
                                              rep("SAS", 3),
                                              rep("SPSS", 3),
                                              rep("SQL", 3),
                                              rep("Stata", 3),
                                              rep("VBA", 3)),
                                   "value" = factor(rep(c("Yes", "No", "Don't Know"), 11),
                                                    levels = c("Yes", "No", "Don't Know")),
                                   "n" = c(1/6, 1/3, 1/2, 1/3, 1/2, 1/6, 1/2, 1/3, 1/6, 1/2, 1/3,
                                           1/6, 1/2, 1/3, 1/6, 1/6, 1/3, 1/2, 1/3, 1/2, 1/6, 1/3,
                                           1/2, 1/6, 1/2, 1/3, 1/6, 1/6, 1/3, 1/2, 1/6, 1/3, 1/2))

  expect_equal(got_knowledge, expected_knowledge)

})

test_that("summarise_coding_tools access output is as expected", {

  got_access <- summarise_coding_tools(dummy_data, "access")

  expected_access <- data.frame("name" = c(rep("C++ / C#", 3),
                                           rep("Java / Scala", 3),
                                           rep("Javascript / Typescript", 3),
                                           rep("Matlab", 3),
                                           rep("Python", 3),
                                           rep("R", 3),
                                           rep("SAS", 3),
                                           rep("SPSS", 3),
                                           rep("SQL", 3),
                                           rep("Stata", 3),
                                           rep("VBA", 3)),
                                "value" = factor(rep(c("Yes", "No", "Don't Know"), 11),
                                                 levels = c("Yes", "No", "Don't Know")),
                                "n" = c(1/3, 1/6, 1/2, 1/2, 1/6, 1/3, 1/6, 1/2, 1/3, 1/6, 5/6,
                                        0, 1/6, 1/2, 1/3, 1/3, 1/6, 1/2, 1/2, 1/6, 1/3, 1/2,
                                        1/6, 1/3, 1/6, 1/2, 1/3, 1/3, 1/6, 1/2, 1/3, 1/6, 1/2))

  expect_equal(got_access, expected_access)

})
