
test_that("summarise_ci works", {

  dummy_data <- data.frame(CI = c(NA,
                                  rep("Yes", 2),
                                  rep("No", 3),
                                  rep("I don't know what continuous integration is", 4)))

  got <- summarise_ci(dummy_data)

  expect_false(any(is.na.data.frame(got)))

  expected <- data.frame(name = c(rep("Continuous Integration Frequency", 3)),
                         value = factor(c("Yes",
                                          "No",
                                          "I don't know what continuous integration is"),
                                        levels = c("Yes",
                                                   "No",
                                                   "I don't know what continuous integration is")),
                         n = c(0.22, 0.33, 0.44))

  expect_equal(got, expected)

})

test_that("Validation checks work", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_ci(dummy_data), "unexpected_input: no column called 'CI'")

})
