test_that("create_tidy_cross_table works", {

  dummy_data <- data.frame(Q1 = c(NA,
                                  "test1",
                                  "test2",
                                  "test3"),
                           Q2 = c(NA,
                                  3,
                                  1,
                                  2))

  col1 <- "Q1"
  col2 <- "Q2"
  levels1 <- c("test1", "test2", "test3")
  levels2 <- c(1, 2, 3)

  expected <- data.frame(Q1 = factor(c(rep("test1", 3),
                                       rep("test2", 3),
                                       rep("test3", 3)),
                                     levels = levels1),
                         Q2 = factor(c(rep(c(1,2,3), 3)),
                                     levels = levels2),
                         n = c(0, 0, 1, 1, 0, 0, 0, 1, 0))

  got <- create_tidy_cross_table(data = dummy_data,
                                 col1 = col1,
                                 col2 = col2,
                                 levels1 = levels1,
                                 levels2 = levels2)

  expect_equal(got, expected)

})
