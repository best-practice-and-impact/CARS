
dummy_data <- data.frame(coding_ability_change = c(NA,
                                                   rep("Significantly worse", 2),
                                                   rep("Slightly worse", 3),
                                                   rep("No change", 4),
                                                   rep("Slightly better", 5),
                                                   rep("Significantly better", 6)),
                         code_freq = c(NA,
                                       rep("Sometimes", 6),
                                       rep("All the time", 5),
                                       rep("Never", 3),
                                       rep("Rarely", 4),
                                       rep("Regularly", 2)))

test_that("summarise_cap_change_by_freq missing data is handled correctly", {

  got <- summarise_cap_change_by_freq(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_cap_change_by_freq output is as expected", {

  got <- summarise_cap_change_by_freq(dummy_data)

  expected <- data.frame(code_freq = factor(c(rep("Never", 5),
                                              rep("Rarely", 5),
                                              rep("Sometimes", 5),
                                              rep("Regularly", 5),
                                              rep("All the time", 5)),
                                            levels = c("Never",
                                                       "Rarely",
                                                       "Sometimes",
                                                       "Regularly",
                                                       "All the time")),
                         coding_ability_change = factor(rep(c("Significantly worse",
                                                              "Slightly worse",
                                                              "No change",
                                                              "Slightly better",
                                                              "Significantly better"), 5),
                                                        levels = c("Significantly worse",
                                                                   "Slightly worse",
                                                                   "No change",
                                                                   "Slightly better",
                                                                   "Significantly better")),
                         n = c(0.00, 0.00, 0.00, 1.00, 0.00,
                               0.00, 0.00, 0.00, 0.00, 1.00,
                               1/3, 0.50, 1/6, 0.00, 0.00,
                               0.00, 0.00, 0.00, 0.00, 1.00,
                               0.00, 0.00, 0.60, 0.40, 0.00))

  expect_equal(got, expected)

})
