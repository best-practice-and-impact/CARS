# Coding tools frequency tables (access or knowledge)

test_that("summarise_languages_by_prof works", {

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
    knowledge_java_scala = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
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

  got <- summarise_languages_by_prof(dummy_data)

  expect_false(any(is.na.data.frame(got)))

  expected <- data.frame(lang = factor(c(rep("R", 8),
                                         rep("SQL", 8),
                                         rep("Python", 8),
                                         rep("SAS", 8),
                                         rep("SPSS", 8),
                                         rep("VBA", 8),
                                         rep("Matlab", 8),
                                         rep("Stata", 8),
                                         rep("JavaScript", 8),
                                         rep("Scala", 8),
                                         rep("C#/C++", 8)),
                                       levels = c("C#/C++", "JavaScript", "Matlab", "Python", "R",
                                                  "SAS", "SPSS", "SQL", "Scala", "Stata", "VBA")),
                         name = factor(rep(c("Data scientists",
                                             "Digital and data (DDAT)",
                                             "Actuaries",
                                             "Economists (GES)",
                                             "Geographers",
                                             "Operational researchers (GORS)",
                                             "Social researchers (GSR)",
                                             "Statisticians (GSG)"), 11),
                                       levels = c("Data scientists",
                                                  "Digital and data (DDAT)",
                                                  "Actuaries",
                                                  "Economists (GES)",
                                                  "Geographers",
                                                  "Operational researchers (GORS)",
                                                  "Social researchers (GSR)",
                                                  "Statisticians (GSG)")),
                         value = c(0.33, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.33,
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
