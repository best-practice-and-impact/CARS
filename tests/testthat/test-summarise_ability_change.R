
dummy_data <- data.frame(first_learned =rep(c(NA,
                                          "Current employment",
                                          "Education",
                                          "Previous public sector employment",
                                          "Previous private sector employment",
                                          "Other"),
                                          times = 6),
                        coding_ability_change = rep(c(NA,
                                                   "It has become significantly worse",
                                                   "It has become slightly worse",
                                                   "It has stayed the same",
                                                   "It has become slightly better",
                                                   "It has become significantly better"),
                                                   each = 6)
)

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
                                          "Stayed the same",
                                          "Slightly better",
                                          "Significantly better"),
                                        levels = c("Significantly worse",
                                                   "Slightly worse",
                                                   "Stayed the same",
                                                   "Slightly better",
                                                   "Significantly better")),
                         n=c(0.2, 0.2, 0.2, 0.2, 0.2))

  expect_equal(got, expected)
})
