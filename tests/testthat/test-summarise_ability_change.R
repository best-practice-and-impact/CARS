
dummy_data <- data.frame(coding_ability_change = c(NA,
                                                   rep("Significantly worse", 2),
                                                   rep("Slightly worse", 3),
                                                   rep("No change", 4),
                                                   rep("Slightly better", 5),
                                                   rep("Significantly better", 6)))

test_that("summarise_ability_change validation works", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_ability_change(dummy_data), "unexpected_input: no column called 'coding_ability_change'")

})

test_that("summarise_ability_change missing data is handled correctly", {

  got <- summarise_ability_change(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_ability_change output is as expected", {

  got <- summarise_ability_change(dummy_data)

  expected <- data.frame(value = factor(c("Significantly worse",
                                          "Slightly worse",
                                          "No change",
                                          "Slightly better",
                                          "Significantly better"),
                                        levels = c("Significantly worse",
                                                   "Slightly worse",
                                                   "No change",
                                                   "Slightly better",
                                                   "Significantly better")),
                         n=c(0.10, 0.15, 0.20, 0.25, 0.30))

  expect_equal(got, expected)
})
