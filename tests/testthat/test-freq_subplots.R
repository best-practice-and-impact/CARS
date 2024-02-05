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
                                      height = 20, width = 20, nrows = 1),
                        "Unexpected input: n_rows should be 2 or greater.")
                    })


got <- freq_subplots(dummy_data, xlab = "x", ylab = "y",
              height = 500, width = 300, nrows = 3,
              y_margin = .3, x_margin = .3, orientation = "v")

for(i in 1:length(unique(dummy_data[[2]]))){
  j = 2*i - 1
  testthat::test_that("expected outputs for vertical chart achieved",
                      {
                        # x values
                        testthat::expect_equal(c(got$x$data[[j]]$x), factor(levels1, levels = levels1))

                        # y values
                        testthat::expect_equal(c(got$x$data[[j]]$y), dummy_data[dummy_data$Q2 == i, "n"])

                        # Bar colors
                        testthat::expect_equal(got$x$data[[j]]$marker$color, "#004556")

                        # Plot orientation
                        testthat::expect_equal(got$x$data[[j]]$orientation, "v")

                        # Title
                        testthat::expect_equal(got$x$data[[j]]$title, factor(i, levels = c(1, 2, 3)))
                      })
}

got <- freq_subplots(dummy_data, xlab = "x", ylab = "y",
                     height = 500, width = 300, nrows = 3,
                     y_margin = .3, x_margin = .3, orientation = "h")

for(i in 1:length(unique(dummy_data[[2]]))){
  j = 2*i - 1
  testthat::test_that("expected outputs for horizontal chart achieved",
                      {
                        # x values
                        testthat::expect_equal(c(got$x$data[[j]]$y), factor(levels1, levels = rev(levels1)))

                        # y values
                        testthat::expect_equal(c(got$x$data[[j]]$x), dummy_data[dummy_data$Q2 == i, "n"])

                        # Bar colors
                        testthat::expect_equal(got$x$data[[j]]$marker$color, "#004556")

                        # Plot orientation
                        testthat::expect_equal(got$x$data[[j]]$orientation, "h")

                        # Title
                        testthat::expect_equal(got$x$data[[j]]$title, factor(i, levels = c(1, 2, 3)))
                      })
}
