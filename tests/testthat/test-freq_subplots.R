levels1 <- c("test1", "test2", "test3")
levels2 <- c(1, 2, 3)

dummy_data <- data.frame(Q1 = factor(c(rep("test1", 3),
                                       rep("test2", 3),
                                       rep("test3", 3)),
                                     levels = levels1),
                         Q2 = factor(c(rep(c(1,2,3), 3)),
                                     levels = levels2),
                         n = c(0, 0, 1, 1, 0, 0, 0, 1, 0))


testthat::test_that("validity check works",
                    {
                      testthat::expect_error(
                        freq_subplots(dummy_data, xlab = "x", ylab = "y",
                                      height = 20, width = 20, nrows = 1))
                    })


got <- freq_subplots(dummy_data, xlab = "x", ylab = "y",
              height = 500, width = 300, nrows = 3,
              y_margin = .3, x_margin = .3, orientation = "v")

testthat::test_that("expected outputs achieved",
                    {
                      testthat::expect_equal(c(got$x$data[[1]]$x), factor(levels1, levels = levels1))
                      testthat::expect_equal(c(got$x$data[[1]]$y), dummy_data[dummy_data$Q2 == 1, "n"])
                    })
