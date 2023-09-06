
dummy_data <- data.frame(management = c(NA,
                                        rep("Yes", 5),
                                        rep("No - I manage people who do not write code", 5),
                                        rep("No - I don't line manage anyone", 5)),
                         coding_ability_change = c(NA,
                                                   rep(c("Significantly worse",
                                                         "Slightly worse",
                                                         "No change",
                                                         "Slightly better",
                                                         "Significantly better"),
                                                   times=3)))

test_that("summarise_cap_change_by_line_manage missing data is handled correctly", {

  got <- summarise_cap_change_by_line_manage(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_cap_change_by_line_manage output is as expected", {

  got <- summarise_cap_change_by_line_manage(dummy_data)

  expected <- data.frame(management = factor(rep(c("Yes",
                                                   "No - I manage people who do not write code",
                                                   "No - I don't line manage anyone"),
                                                 each=5),
                                             levels = c("Yes",
                                                        "No - I manage people who do not write code",
                                                        "No - I don't line manage anyone")),
                         coding_ability_change = factor(rep(c("Significantly worse",
                                                        "Slightly worse",
                                                        "No change",
                                                        "Slightly better",
                                                        "Significantly better"), 3),
                                                  levels = c("Significantly worse",
                                                             "Slightly worse",
                                                             "No change",
                                                             "Slightly better",
                                                             "Significantly better")),
                         n = c(0.20, 0.20, 0.20, 0.20, 0.20,
                               0.20, 0.20, 0.20, 0.20, 0.20,
                               0.20, 0.20, 0.20, 0.20, 0.20))

  expect_equal(got, expected)

})
