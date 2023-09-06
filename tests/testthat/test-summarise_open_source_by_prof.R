# Coding tools frequency tables (access or knowledge)

dummy_data <- data.frame(
  prac_use_open_source = c(NA,
                           rep(c("I don't understand this question",
                               "Never",
                               "Rarely",
                               "Sometimes",
                               "Regularly",
                               "All the time"), 2)),
  prac_open_source_own = rep("Sometimes", 13),
  prac_version_control = rep("Sometimes", 13),
  prac_review = rep("Sometimes", 13),
  prac_functions = rep("Sometimes", 13),
  prac_unit_test = rep("Sometimes", 13),
  prac_package = rep("Sometimes", 13),
  prac_dir_structure = rep("Sometimes", 13),
  prac_style = rep("Sometimes", 13),
  prac_automated_QA = rep("Sometimes", 13),
  prac_AQUA_book = rep("Sometimes", 13),
  prof_DS = c(NA, rep(c("Yes", "No"), each=6)),
  prof_DDAT = c(NA, rep(c("Yes", "No"), each=6)),
  prof_GAD = c(NA, rep(c("Yes", "No"), each=6)),
  prof_GES = c(NA, rep(c("Yes", "No"), each=6)),
  prof_geog = c(NA, rep(c("Yes", "No"), each=6)),
  prof_GORS = c(NA, rep(c("Yes", "No"), each=6)),
  prof_GSR = c(NA, rep(c("Yes", "No"), each=6)),
  prof_GSG = c(NA, rep(c("Yes", "No"), each=6)))

test_that("summarise_open_source_by_prof missing data is handled correctly", {

  got <- summarise_open_source_by_prof(dummy_data)


  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_open_source_by_prof output is as expected", {

  got <- summarise_open_source_by_prof(dummy_data)

  expected <- data.frame(name = rep(c("Data scientists",
                                      "Digital and data (DDAT)",
                                      "Actuaries",
                                      "Economists (GES)",
                                      "Geographers",
                                      "Operational researchers (GORS)",
                                      "Social researchers (GSR)",
                                      "Statisticians (GSG)"), each=6),
                         value = factor(rep(c("I don't understand this question",
                                              "Never",
                                              "Rarely",
                                              "Sometimes",
                                              "Regularly",
                                              "All the time"), 8),
                                        levels = c("I don't understand this question",
                                                   "Never",
                                                   "Rarely",
                                                   "Sometimes",
                                                   "Regularly",
                                                   "All the time")),
                         n = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6,
                               1/6, 1/6, 1/6, 1/6, 1/6, 1/6,
                               1/6, 1/6, 1/6, 1/6, 1/6, 1/6,
                               1/6, 1/6, 1/6, 1/6, 1/6, 1/6,
                               1/6, 1/6, 1/6, 1/6, 1/6, 1/6,
                               1/6, 1/6, 1/6, 1/6, 1/6, 1/6,
                               1/6, 1/6, 1/6, 1/6, 1/6, 1/6,
                               1/6, 1/6, 1/6, 1/6, 1/6, 1/6))

  expect_equal(got, expected)

})
