dummy_data <- data.frame(git = c(NA,
                                 rep("Yes", 2),
                                 rep("No", 3),
                                 rep("I don't know", 4)))

dummy_config <- list(git = list(levels = c("Yes", "No", "I don't know")))


test_that("summarise_git output is as expected", {

  got <- summarise_git(dummy_data, dummy_config, question = "git")

  expected <- data.frame(value = factor(c("Yes",
                                          "No",
                                          "I don't know"),
                                        levels = c("Yes",
                                                   "No",
                                                   "I don't know")),
                         n = c(2/9, 1/3, 4/9),
                         count = c(2, 3, 4),
                         sample = c(9, 9, 9))


  testthat::expect_equal(got, expected)

})

test_that("summarise_access_git missing data is handled correctly", {

  got <- summarise_git(dummy_data, dummy_config, question = "git")

  expect_false(any(is.na.data.frame(got)))

})
