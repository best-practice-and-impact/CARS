
test_that("summarise_coding_practises works", {

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

  got <- summarise_coding_practices(dummy_data)

  expect_false(any(is.na.data.frame(got)))

  expected <- data.frame(name = c(rep("Code my team writes is reviewed by a colleague", 6),
                                  rep("I collect my code and supporting material into packages", 6),
                                  rep("I follow a standard directory structure when programming", 6),
                                  rep("I follow coding guidelines or style guides when programming", 6),
                                  rep("I unit test my code", 6),
                                  rep("I use a source code version control system e.g. Git", 6),
                                  rep("I use open source software when programming", 6),
                                  rep("I write code to automatically quality assure data", 6),
                                  rep("I write repetitive elements in my code as functions", 6),
                                  rep("My team applies the principles set out in the Aqua book when carrying out analysis as code", 6),
                                  rep("My team open sources its code", 6)),
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
                         n = c(0.00, 0.17, 0.00, 0.00, 0.50, 0.33, 0.00, 0.50, 0.17, 0.33, 0.00,
                               0.00, 0.00, 0.17, 0.33, 0.50, 0.00, 0.00, 0.00, 0.33, 0.50, 0.17,
                               0.00, 0.00, 0.00, 0.17, 0.33, 0.00, 0.00, 0.50, 0.00, 0.17, 0.50,
                               0.00, 0.00, 0.33, 0.00, 0.60, 0.00, 0.40, 0.00, 0.00, 0.00, 0.17,
                               0.00, 0.33, 0.50, 0.00, 0.50, 0.33, 0.17, 0.00, 0.00, 0.00, 0.50,
                               0.33, 0.00, 0.17, 0.00, 0.00, 0.33, 0.00, 0.00, 0.50, 0.00, 0.17))

  expect_equal(got, expected)

})
