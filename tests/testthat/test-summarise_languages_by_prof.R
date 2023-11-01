# Coding tools frequency tables (access or knowledge)

dummy_data <- data.frame(
  code_freq = c(rep("Sometimes", 2), rep("All the time", 2), "Never", NA),
  knowledge_R = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  knowledge_SQL = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  knowledge_SAS = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  knowledge_VBA = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  knowledge_python = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  knowledge_SPSS = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  knowledge_stata = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  knowledge_matlab = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  prof_DS = c(rep("Yes", 3), rep("No", 3)),
  prof_DDAT = c("No", rep("Yes", 4), rep("No", 1)),
  prof_GAD = c(rep("No", 2), rep("Yes", 4)),
  prof_GES = c(rep("No", 2), rep("Yes", 2), rep("No", 2)),
  prof_geog = c(rep("No", 3), rep("Yes", 3)),
  prof_GORS = c(rep("No", 1), rep("Yes", 5)),
  prof_GSR = c(rep("Yes", 1), rep("No", 5)),
  prof_GSG = c(rep("Yes", 6)))

test_that("summarise_languages_by_prof missing data is handled correctly", {

  got <- summarise_languages_by_prof(dummy_data)


  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_languages_by_prof output is as expected", {

  got <- summarise_languages_by_prof(dummy_data)

  expected <- data.frame(lang = rep(c("Matlab",
                                      "Python",
                                      "R",
                                      "SAS",
                                      "SPSS",
                                      "SQL",
                                      "Stata",
                                      "VBA"), 8),
                         prof = c(rep("Data scientists", 8),
                                  rep("Digital and data (DDAT)", 8),
                                  rep("Actuaries", 8),
                                  rep("Economists (GES)", 8),
                                  rep("Geographers", 8),
                                  rep("Operational researchers (GORS)", 8),
                                  rep("Social researchers (GSR)", 8),
                                  rep("Statisticians (GSG)", 8)),
                         n = c(1, 1, 1/3, 2/3, 2/3, 1, 1/3, 1/3,
                               1/2, 1/2, 0, 1/4, 1/4, 1/2, 0, 0,
                               1/4, 1/4, 0, 0, 0, 1/4, 0, 0,
                               1/2, 1/2, 0, 0, 0, 1/2, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 0,
                               2/5, 2/5, 0, 1/5, 1/5, 2/5, 0, 0,
                               1, 1, 1, 1, 1, 1, 1, 1,
                               1/2, 1/2, 1/6, 1/3, 1/3, 1/2, 1/6, 1/6))

  expect_equal(got, expected)

})
