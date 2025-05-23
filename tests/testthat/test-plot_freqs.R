
dummy_data <- data.frame(Q1 = factor(c("This is test number one", "This is test 2", "test3"),
                                     levels = c("This is test number one", "This is test 2", "test3")),
                         n = c(0.5, 0.2, 0.8),
                         count = c(5, 2, 8),
                         sample = c(10, 10, 10))


test_that("validity checks work",
                    {
                      testthat::expect_error(plot_freqs(dummy_data, colour = c("blue", "green")), "Unexpected input - colour should be a single colour name.")
                      testthat::expect_error(plot_freqs(dummy_data, colour = 1), "Unexpected input - colour should be a single colour name.")
                      testthat::expect_error(plot_freqs(as.list(dummy_data)), "Unexpected input - data is not a data.frame.")
                      testthat::expect_error(plot_freqs(dplyr::mutate(dummy_data, Q2 = Q1)), "Unexpected input - data does not contain four columns.")
                      testthat::expect_error(plot_freqs(dplyr::mutate(dummy_data, n = as.character(n))), "Unexpected input - data column 2 is not numeric.")
                      testthat::expect_error(plot_freqs(dummy_data, xlab = 1), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_freqs(dummy_data, ylab = 1), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_freqs(dummy_data, xlab = c("1", "2")), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_freqs(dummy_data, ylab = c("1", "2")), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_freqs(dummy_data, font_size = "x"), "Unexpected input - font_size is not numeric.")
                    })

test_that("expected outputs for vertical chart achieved",
                    {

                      got <- plot_freqs(dummy_data, xlab = "x", ylab = "y",
                                        break_q_names_col = TRUE,
                                        type = "bar",
                                        orientation = "v")

                      # x and y values
                      testthat::expect_equal(c(got$x$attrs[[1]]$x), factor(c("This is test<br>number one", "This is test<br>2", "test3"),
                                                                              levels = c("This is test<br>number one", "This is test<br>2", "test3")))
                      testthat::expect_equal(c(got$x$attrs[[1]]$y), c(0.5, 0.2, 0.8))

                      # Bar colors
                      testthat::expect_equal(got$x$attrs[[1]]$marker$color, "#12436D")

                      # Plot orientation
                      testthat::expect_equal(got$x$attrs[[1]]$orientation, "v")

                      # Sample size
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$text, "Sample size = 10")

                      # Axis labels
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$title, "x")
                      testthat::expect_equal(got$x$layoutAttrs[[2]]$annotations$text, "y")

                      # Font sizes
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$titlefont$size, 14.4)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$tickfont$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$yaxis$titlefont$size, 14.4)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$yaxis$tickfont$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$hoverlabel$font$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$font$size, 12)

                    })

test_that("expected outputs for horizontal chart achieved",
                    {

                      got <- plot_freqs(dummy_data, xlab = "x", ylab = "y",
                                        break_q_names_col = TRUE,
                                        type = "line",
                                        orientation = "h")

                      # x and y values
                      testthat::expect_equal(c(got$x$attrs[[1]]$y), factor(c("This is test<br>number one", "This is test<br>2", "test3"),
                                                                           levels = rev(c("This is test<br>number one", "This is test<br>2", "test3"))))
                      testthat::expect_equal(c(got$x$attrs[[1]]$x), c(0.5, 0.2, 0.8))

                      # Bar colors
                      testthat::expect_equal(got$x$attrs[[1]]$marker$color, "#12436D")

                      # Plot orientation
                      testthat::expect_equal(got$x$attrs[[1]]$orientation, "h")

                      # Sample size
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$text, "Sample size = 10")

                      # Axis labels
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$title, "y")
                      testthat::expect_equal(got$x$layoutAttrs[[2]]$annotations$text, "x")

                      # Font sizes
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$titlefont$size, 14.4)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$tickfont$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$yaxis$titlefont$size, 14.4)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$yaxis$tickfont$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$hoverlabel$font$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$font$size, 12)

                    })
