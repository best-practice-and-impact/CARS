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
  knowledge_matlab = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_matlab = c("Yes", rep("No", 5), rep("Don't Know", 0)) # Used to check zero counts aren't missing
)

test_that("summarise_coding_tools missing data is handled correctly", {

  dummy_data[1,] <- NA

  got_missing <- summarise_coding_tools(dummy_data, "knowledge")

  expect_false(any(is.na(got_missing)))

})

test_that("summarise_coding_tools knowledge output is as expected", {

  got_knowledge <- summarise_coding_tools(dummy_data, "knowledge")

  expected_knowledge <- data.frame("name" = rep(c("Matlab",
                                                  "Python",
                                                  "R",
                                                  "SAS",
                                                  "SPSS",
                                                  "SQL",
                                                  "Stata",
                                                  "VBA"), each=3),
                                   "value" = factor(rep(c("Yes", "Don't Know", "No"), 8),
                                                    levels = c("Yes", "Don't Know", "No")),
                                   "n" = c(1/2, 1/6, 1/3, 1/2, 1/6, 1/3, 1/6, 1/2,
                                           1/3, 1/3, 1/6, 1/2, 1/3, 1/6, 1/2, 1/2,
                                           1/6, 1/3, 1/6, 1/2, 1/3, 1/6, 1/2, 1/3))

  expect_equal(got_knowledge, expected_knowledge)

})

test_that("summarise_coding_tools access output is as expected", {

  got_access <- summarise_coding_tools(dummy_data, "access")

  expected_access <- data.frame("name" = rep(c("Matlab",
                                               "Python",
                                               "R",
                                               "SAS",
                                               "SPSS",
                                               "SQL",
                                               "Stata",
                                               "VBA"), each=3),
                                "value" = factor(rep(c("Yes", "Don't Know", "No"), 8),
                                                 levels = c("Yes", "Don't Know", "No")),
                                "n" = c(1/6, 0, 5/6, 1/6, 1/3, 1/2, 1/3, 1/2,
                                        1/6, 1/2, 1/3, 1/6, 1/2, 1/3, 1/6, 1/6,
                                        1/3, 1/2, 1/3, 1/2, 1/6, 1/3, 1/2, 1/6))

  expect_equal(got_access, expected_access)

})
