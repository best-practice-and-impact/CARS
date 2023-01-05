levels1 <- c("test1", "test2", "test3")

dummy_data <- data.frame(Q1 = factor(c("test1","test2","test3"),
                                     levels = levels1),
                         n = c(0.5, 0.2, 0.8))


testthat::test_that("validity checks work",
                    {
                      testthat::expect_error(plot_freqs(dummy_data, bar_colour = c("blue", "green")))
                      testthat::expect_error(plot_freqs(dummy_data, bar_colour = 1))
                      testthat::expect_error(plot_freqs(as.list(dummy_data)))
                      testthat::expect_error(plot_freqs(dplyr::mutate(dummy_data, Q2 = Q1)))
                      testthat::expect_error(plot_freqs(dplyr::mutate(dummy_data, n = as.character(n))))
                      testthat::expect_error(plot_freqs(dummy_data, xlab = 1))
                      testthat::expect_error(plot_freqs(dummy_data, ylab = 1))
                      testthat::expect_error(plot_freqs(dummy_data, xlab = c("1", "2")))
                      testthat::expect_error(plot_freqs(dummy_data, ylab = c("1", "2")))
                      testthat::expect_error(plot_freqs(dummy_data, font_size = "x"))
                    })



# got <- plot_freqs(dummy_data, xlab = "x", ylab = "y")
#
# testthat::test_that("expected outputs achieved",
#                     {
#                       testthat::expect_equal(c(got$x$data[[1]]$x), factor(levels1, levels = levels1))
#                       testthat::expect_equal(c(got$x$data[[1]]$y), dummy_data[dummy_data$Q2 == 1, "n"])
#                     })
