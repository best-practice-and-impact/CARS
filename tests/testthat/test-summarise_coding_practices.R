
dummy_data <- data.frame(prac_use_open_source = c(rep("Never", 3), rep("Sometimes", 2), rep(NA, 1)),
                         prac_open_source_own = c(rep("Sometimes", 3), rep("I don't understand this question", 2), rep("All the time", 1)),
                         prac_version_control = c(rep("Rarely", 3), rep("All the time", 2), rep("Never", 1)),
                         prac_review = c(rep("Regularly", 3), rep("All the time", 2), rep("Never", 1)),
                         prac_functions = c(rep("I don't understand this question", 3), rep("Never", 2), rep("Rarely", 1)),
                         prac_unit_test = c(rep("All the time", 3), rep("Rarely", 2), rep("Never", 1)),
                         prac_package = c(rep("Never", 3), rep("Sometimes", 2), rep("Rarely", 1)),
                         prac_dir_structure = c(rep("Sometimes", 3), rep("Rarely", 2), rep("Never", 1)),
                         prac_style = c(rep("Rarely", 3), rep("Never", 2), rep("Sometimes", 1)),
                         prac_automated_QA = c(rep("Regularly", 3), rep("Sometimes", 2), rep("Never", 1)),
                         prac_AQUA_book = c(rep("I don't understand this question", 3), rep("Never", 2), rep("Sometimes", 1)))

test_that("summarise_coding_practises missing data is handled correctly", {

  got <- summarise_coding_practices(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_coding_practises output is as expected", {

  got <- summarise_coding_practices(dummy_data)

  expected <- data.frame(name = rep(sort(c("Use open source software",
                                           "Open source own code",
                                           "Version control",
                                           "Code review",
                                           "Functions",
                                           "Unit testing",
                                           "Packaging code",
                                           "Standard directory structure",
                                           "Coding guidelines / Style guides",
                                           "Automated data quality assurance",
                                           "Apply AQUA book principles with analysis code")), each=6),
                         value = factor(rep(c("I don't understand this question",
                                              "Never",
                                              "Rarely",
                                              "Sometimes",
                                              "Regularly",
                                              "All the time"), 11),
                                        levels = c("I don't understand this question",
                                                                      "Never",
                                                                      "Rarely",
                                                                      "Sometimes",
                                                                      "Regularly",
                                                                      "All the time")),
                         n = c(1/2, 1/3, 0, 1/6, 0, 0,
                               0, 1/6, 0, 1/3, 1/2, 0,
                               0, 1/6, 0, 0, 1/2, 1/3,
                               0, 1/3, 1/2,  1/6, 0, 0,
                               1/2, 1/3, 1/6, 0, 0, 0,
                               1/3, 0, 0, 1/2, 0, 1/6,
                               0, 1/2, 1/6, 1/3, 0, 0,
                               0, 1/6, 1/3, 1/2, 0, 0,
                               0, 1/6, 1/3, 0, 0, 1/2,
                               0, 3/5, 0, 2/5, 0, 0,
                               0, 1/6, 1/2, 0, 0, 1/3))

  expect_equal(got, expected)

})
