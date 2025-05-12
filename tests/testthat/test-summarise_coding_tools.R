dummy_data <- data.frame(
  knowledge_R = c("Yes", "No", "Not required for my work"),
  access_R = c("Yes", "No", "Don't know"),
  knowledge_SQL = c("Yes", "No", "Not required for my work"),
  access_SQL = c("Yes", "No", "Don't know"),
  knowledge_SAS = c("Yes", "No", "Not required for my work"),
  access_SAS = c("Yes", "No", "Don't know"),
  knowledge_VBA = c("Yes", "No", "Not required for my work"),
  access_VBA = c("Yes", "No", "Don't know"),
  knowledge_python = c("Yes", "No", "Not required for my work"),
  access_python = c("Yes", "No", "Don't know"),
  knowledge_SPSS = c("Yes", "No", "Not required for my work"),
  access_SPSS = c("Yes", "No", "Don't know"),
  knowledge_stata = c("Yes", "No", "Not required for my work"),
  access_stata = c("Yes", "No", "Don't know"),
  knowledge_matlab = c("Yes", "No", "No"),
  access_matlab = c("Yes", "No", "No") # Used to check zero counts aren't missing
)

test_that("summarise_coding_tools missing data is handled correctly", {

  dummy_data[1,] <- NA

  got_missing <- summarise_coding_tools(dummy_data, config, question = "coding_tools_knowledge")

  expect_false(any(is.na(got_missing)))

})

test_that("summarise_coding_tools knowledge output is as expected", {

  got_knowledge <- summarise_coding_tools(dummy_data, config, question = "coding_tools_knowledge")

  expected_knowledge <- data.frame(name = rep(c("Python",
                                                "R",
                                                "Matlab",
                                                "SAS",
                                                "SPSS",
                                                "SQL",
                                                "Stata",
                                                "VBA"), each=3),
                                   value = factor(rep(c("Yes", "No", "Not required for my work"), 8),
                                                  levels = c("Yes", "No", "Not required for my work")),
                                   n = c(rep(1/3, times=6), 1/3, 2/3, 0, rep(1/3, times=15)),
                                   count = c(rep(1, times=6), 1, 2, 0, rep(1, times=15)),
                                   sample = 3
  )

  expect_equal(got_knowledge, expected_knowledge)

})

test_that("summarise_coding_tools access output is as expected", {

  got_access <- summarise_coding_tools(dummy_data,  config, question = "coding_tools_access")

  expected_access <- data.frame(name = rep(c("Python",
                                             "R",
                                             "Matlab",
                                             "SAS",
                                             "SPSS",
                                             "SQL",
                                             "Stata",
                                             "VBA"), each=3),
                                value = factor(rep(c("Yes", "No", "Don't know"), 8),
                                               levels = c("Yes", "No", "Don't know")),
                                n = c(rep(1/3, times=6), 1/3, 2/3, 0, rep(1/3, times=15)),
                                count = c(rep(1, times=6), 1, 2, 0, rep(1, times=15)),
                                sample = 3
  )

  expect_equal(got_access, expected_access)

})
