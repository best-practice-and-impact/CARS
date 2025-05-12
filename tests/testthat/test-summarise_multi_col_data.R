dummy_data <- data.frame(col1 = c(NA,
                                rep("Yes", 2),
                                rep("No", 3),
                                rep("I don't know", 4)),
                         col2 = c(NA,
                                rep("Yes", 3),
                                rep("No", 4),
                                rep("I don't know", 2)))

dummy_config <- list(q1 = list(levels = c("Yes", "No", "I don't know"),
                               cols = c(list(col1 = "col1"), list(col2 ="col2"))))


test_that("summarise_data output is as expected", {

  got <- CARS::summarise_data(dummy_data, dummy_config, question = "q1")

  expected <- data.frame(name = c(rep("col1", 3),
                                  rep("col2", 3)),
                         value = factor(c("Yes",
                                          "No",
                                          "I don't know"),
                                        levels = c("Yes",
                                                   "No",
                                                   "I don't know")),
                         n = c(2/9, 1/3, 4/9, 1/3, 4/9, 2/9),
                         count = c(2, 3, 4, 3, 4, 2),
                         sample = 9)


  expect_equal(got, expected)

})
