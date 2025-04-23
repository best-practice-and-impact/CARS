dummy_data <- data.frame(q1 = c(NA,
                                    rep("Yes", 2),
                                    rep("No", 3),
                                    rep("I don't know", 4)))

dummy_config <- list(q1 = list(levels = c("Yes", "No", "I don't know")))


testthat::test_that("summarise_data output is as expected", {

  got <- CARS::summarise_data(dummy_data, dummy_config, question = "q1")

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
