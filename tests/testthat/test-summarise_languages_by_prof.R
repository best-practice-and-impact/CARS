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
  knowledge_JS = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  knowledge_java = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  knowledge_C = c("Yes", rep("No", 2), rep("Don't Know", 3)),
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

  expected <- data.frame(prof = c(rep("Data scientists", 11),
                                  rep("Digital and data (DDAT)", 11),
                                  rep("Actuaries", 11),
                                  rep("Economists (GES)", 11),
                                  rep("Geographers", 11),
                                  rep("Operational researchers (GORS)", 11),
                                  rep("Social researchers (GSR)", 11),
                                  rep("Statisticians (GSG)", 11)),
                         lang = rep(c("C++ / C#",
                                      "Java / Scala",
                                      "Javascript / Typescript",
                                      "Matlab",
                                      "Python",
                                      "R",
                                      "SAS",
                                      "SPSS",
                                      "SQL",
                                      "Stata",
                                      "VBA"), 8),
                         n = c(0.33, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.33,
                               0.23, 0.15, 0.08, 0.08, 0.00, 0.15, 0.08, 0.23,
                               0.23, 0.15, 0.08, 0.08, 0.00, 0.15, 0.08, 0.23,
                               0.29, 0.14, 0.00, 0.00, 0.00, 0.14, 0.14, 0.29,
                               0.29, 0.14, 0.00, 0.00, 0.00, 0.14, 0.14, 0.29,
                               0.33, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.33,
                               0.23, 0.15, 0.08, 0.08, 0.00, 0.15, 0.08, 0.23,
                               0.33, 0.00, 0.00 ,0.00, 0.00, 0.00, 0.33, 0.33,
                               0.23, 0.15, 0.08, 0.08, 0.00, 0.15, 0.08, 0.23,
                               0.29, 0.14, 0.00, 0.00, 0.00, 0.14, 0.14, 0.29,
                               0.33, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.33))

  expect_equal(got, expected)

})
