
dummy_data <- data.frame(CI = c(NA,
                                rep("Yes", 2),
                                rep("No", 3),
                                rep("I don't know what continuous integration is", 4)))

test_that("summarise_ci validation works", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_ci(dummy_data), "unexpected_input: no column called 'CI'")

})

test_that("summarise_ci missing data is handled correctly", {

  got <- summarise_ci(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_ci output is as expected", {

  got <- summarise_ci(dummy_data)

  expected <- data.frame(value = factor(c("Yes",
                                          "No",
                                          "I don't know what continuous integration is"),
                                        levels = c("Yes",
                                                   "No",
                                                   "I don't know what continuous integration is")),
                         n = c(2/9, 1/3, 4/9))

  expect_equal(got, expected)

})
