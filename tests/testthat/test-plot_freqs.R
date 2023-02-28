
dummy_data <- data.frame(Q1 = factor(c("This is test number one", "This is test 2", "test3"),
                                     levels = c("This is test number one", "This is test 2", "test3")),
                         n = c(0.5, 0.2, 0.8))


testthat::test_that("validity checks work",
                    {
                      testthat::expect_error(plot_freqs(dummy_data, bar_colour = c("blue", "green")), "Unexpected input - bar_colour should be a single colour name.")
                      testthat::expect_error(plot_freqs(dummy_data, bar_colour = 1), "Unexpected input - bar_colour should be a single colour name.")
                      testthat::expect_error(plot_freqs(as.list(dummy_data)), "Unexpected input - data is not a data.frame.")
                      testthat::expect_error(plot_freqs(dplyr::mutate(dummy_data, Q2 = Q1)), "Unexpected input - data does not contain two columns.")
                      testthat::expect_error(plot_freqs(dplyr::mutate(dummy_data, n = as.character(n))), "Unexpected input - data column 2 is not numeric.")
                      testthat::expect_error(plot_freqs(dummy_data, xlab = 1), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_freqs(dummy_data, ylab = 1), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_freqs(dummy_data, xlab = c("1", "2")), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_freqs(dummy_data, ylab = c("1", "2")), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_freqs(dummy_data, font_size = "x"), "Unexpected input - font_size is not numeric.")
                    })

got <- plot_freqs(dummy_data, n = 100, xlab = "x", ylab = "y", break_q_names_col = TRUE)

testthat::test_that("expected outputs achieved",
                    {
                      # x and y values
                      testthat::expect_equal(c(got$x$attrs[[1]][[1]]), factor(c("This is test<br>number one", "This is test<br>2", "test3"),
                                                                              levels = c("This is test<br>number one", "This is test<br>2", "test3")))
                      testthat::expect_equal(c(got$x$attrs[[1]][[2]]), c(0.5, 0.2, 0.8))

                      # Bar colors
                      testthat::expect_equal(got$x$attrs[[1]][[3]][[1]], "#004556")

                      # Plot orientation
                      testthat::expect_equal(got$x$attrs[[1]][[4]], "v")

                      # Sample size
                      testthat::expect_equal(got$x$layoutAttrs[[1]][[5]][[3]], "Sample size = 100")

                      # Axis labels
                      testthat::expect_equal(got$x$layoutAttrs[[1]][[1]][[1]], "x")
                      testthat::expect_equal(got$x$layoutAttrs[[2]][[1]][[1]], "y")

                      # Font sizes
                      testthat::expect_equal(got$x$layoutAttrs[[1]][[1]][[2]][[1]], 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]][[1]][[3]][[1]], 14.4)
                      testthat::expect_equal(got$x$layoutAttrs[[1]][[2]][[2]][[1]], 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]][[2]][[3]][[1]], 14.4)
                      testthat::expect_equal(got$x$layoutAttrs[[1]][[4]][[2]][[1]], 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]][[5]][[11]][[1]], 12)

                    })
