dummy_data <- data.frame(CS_grade = c(NA,
                                      rep("Higher Executive Officer (or equivalent)", 10),
                                      rep("Senior Executive Officer (or equivalent)", 10),
                                      rep("Grade 7 (or equivalent)", 5),
                                      rep("Grade 6 (or equivalent)", 5)),
                         coding_ability_change = c(NA,
                                                   rep(c("Significantly worse",
                                                       "Slightly worse",
                                                       "No change",
                                                       "Slightly better",
                                                       "Significantly better"),
                                                     times = 6)))

test_that("summarise_cap_change_by_CS_grade missing data is handled correctly", {

  got <- summarise_cap_change_by_CS_grade(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_cap_change_by_CS_grade output is as expected", {

  got <- summarise_cap_change_by_CS_grade(dummy_data)

  expected <- data.frame(CS_grade = factor(rep(c("Higher Executive Officer (or equivalent)",
                                                  "Senior Executive Officer (or equivalent)",
                                                  "Grade 6 and 7"),
                                           each = 5),
                                           levels = c("Higher Executive Officer (or equivalent)",
                                                     "Senior Executive Officer (or equivalent)",
                                                     "Grade 6 and 7")),
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
