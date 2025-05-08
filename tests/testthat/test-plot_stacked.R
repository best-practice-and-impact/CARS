
dummy_data <- data.frame(Q1 = rep(c("This is test number one",
                                    "This is test 2",
                                    "test3"), each = 3),
                         Q2 = factor(rep(c(1, 2, 3), 3),
                                     levels = c(1, 2, 3)),
                         n = c(0.1, 0.3, 0.6, 0.5, 0.25, 0.25, 0.33, 0.33, 0.34),
                         count = c(10, 30, 60, 50, 25, 25, 33, 33, 34),
                         sample = c(100, 100, 100, 100, 100, 100, 100, 100, 100))

test_that("validity checks work",
                    {
                      testthat::expect_error(plot_stacked(as.list(dummy_data)), "Unexpected input - data is not a data.frame.")
                      testthat::expect_error(plot_stacked(dplyr::mutate(dummy_data, Q3 = Q1)), "Unexpected input - data should have five columns.")
                      testthat::expect_error(plot_stacked(dummy_data, xlab = 1), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_stacked(dummy_data, ylab = 1), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_stacked(dummy_data, xlab = c("1", "2")), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_stacked(dummy_data, ylab = c("1", "2")), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_stacked(dummy_data, font_size = "x"), "Unexpected input - font_size is not numeric.")
                    })

test_that("expected outputs for horizontal chart achieved",
                    {

                      got <- plot_stacked(dummy_data, n = 100, xlab = "x", ylab = "y",
                                          orientation = "h",
                                          break_q_names_col = TRUE)

                      # x and y values
                      testthat::expect_equal(c(got$x$attrs[[1]]$y), factor(rep(c("This is test<br>number one", "This is test<br>2", "test3"), each = 3),
                                                                           levels = c("test3", "This is test<br>2", "This is test<br>number one")))
                      testthat::expect_equal(c(got$x$attrs[[1]]$x), c(0.1, 0.3, 0.6, 0.5, 0.25, 0.25, 0.33, 0.33, 0.34))

                      # Bar colors
                      testthat::expect_equal(got$x$attrs[[1]]$marker$color, c("#12436D", "#12436D", "#12436D","#AFAFAF", "#AFAFAF", "#AFAFAF", "#F46A25", "#F46A25", "#F46A25"))

                      # Plot orientation
                      testthat::expect_equal(got$x$attrs[[1]]$orientation, "h")

                      # Sample size
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$text, "Sample size = 100")

                      # Axis labels
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$title, "y")
                      testthat::expect_equal(got$x$layoutAttrs[[2]]$annotations$text, "x")

                      # Font sizes
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$legend$font$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$titlefont$size, 14.4)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$tickfont$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$yaxis$titlefont$size, 14.4)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$yaxis$tickfont$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$hoverlabel$font$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$font$size, 12)

                    })



test_that("expected outputs for vertical chart achieved",
                    {

                      got <- plot_stacked(dummy_data, n = 100, xlab = "x", ylab = "y",
                                          orientation = "v",
                                          type = "line",
                                          break_q_names_col = TRUE)

                      # x and y values
                      testthat::expect_equal(c(got$x$attrs[[1]]$x), factor(rep(c("This is test<br>number one", "This is test<br>2", "test3"), each = 3),
                                                                           levels = rev(c("test3", "This is test<br>2", "This is test<br>number one"))))
                      testthat::expect_equal(c(got$x$attrs[[1]]$y), c(0.1, 0.3, 0.6, 0.5, 0.25, 0.25, 0.33, 0.33, 0.34))

                      # Bar colors
                      testthat::expect_equal(got$x$attrs[[1]]$marker$color, c("#12436D", "#12436D", "#12436D","#AFAFAF", "#AFAFAF", "#AFAFAF", "#F46A25", "#F46A25", "#F46A25"))

                      # Plot orientation
                      testthat::expect_equal(got$x$attrs[[1]]$orientation, "v")

                      # Sample size
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$text, "Sample size = 100")

                      # Axis labels
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$title, "x")
                      testthat::expect_equal(got$x$layoutAttrs[[2]]$annotations$text, "y")

                      # Font sizes
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$legend$font$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$titlefont$size, 14.4)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$xaxis$tickfont$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$yaxis$titlefont$size, 14.4)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$yaxis$tickfont$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$hoverlabel$font$size, 12)
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$font$size, 12)

                    })
