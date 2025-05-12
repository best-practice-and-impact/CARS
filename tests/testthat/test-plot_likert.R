
dummy_data <- data.frame(Q1 = rep(c("test1",
                                    "test2",
                                    "test3"), each = 3),
                         Q2 = factor(rep(c(1, 2, 3), 3),
                                     levels = c(1, 2, 3)),
                         n = c(0.1, 0.3, 0.6, 0.5, 0.25, 0.25, 0.33, 0.33, 0.34),
                         count = c(10, 30, 60, 50, 25, 25, 33, 33, 34),
                         sample = c(100, 100, 100, 100, 100, 100, 100, 100, 100))

test_that("validity checks work",
                    {
                      testthat::expect_error(plot_likert(as.list(dummy_data)), "Unexpected input - data is not a data.frame.")
                      testthat::expect_error(plot_likert(dplyr::mutate(dummy_data, Q3 = Q1)), "Unexpected input - data should have five columns.")
                      testthat::expect_error(plot_likert(dummy_data, xlab = 1), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_likert(dummy_data, ylab = 1), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_likert(dummy_data, xlab = c("1", "2")), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_likert(dummy_data, ylab = c("1", "2")), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_likert(dummy_data, font_size = "x"), "Unexpected input - font_size is not numeric.")
                      testthat::expect_error(plot_likert(dummy_data, neutral_mid = "x"), "Unexpected input - neutral_mid is not logical.")
                      testthat::expect_error(plot_likert(dummy_data, mid = "2"), "Unexpected input - mid is not numeric.")
                      testthat::expect_error(plot_likert(dummy_data, mid = 1), "Unexpected input - mid is smaller than 2.")
                      testthat::expect_error(plot_likert(dummy_data, mid = 3), "Unexpected input - mid >= the number of answers.")
                    })

got <- plot_likert(dummy_data, mid = 2, n = 100, xlab = "x", ylab = "y")


test_that("expected outputs achieved",
                    {
                      # x and y values
                      testthat::expect_equal(c(got$x$attrs[[1]]$y), factor(rep(c("test1", "test2", "test3"), each = 3),
                                                                           levels = c("test3", "test2", "test1")))
                      testthat::expect_equal(c(got$x$attrs[[1]]$x), c(0.1, 0.3, 0.6, 0.5, 0.25, 0.25, 0.33, 0.33, 0.34))

                      # Bar colors
                      testthat::expect_equal(got$x$attrs[[1]]$marker$color, c("#12436D", "#12436D", "#12436D","#AFAFAF", "#AFAFAF", "#AFAFAF", "#F46A25", "#F46A25", "#F46A25"))

                      # Plot orientation
                      testthat::expect_equal(got$x$attrs[[1]]$orientation, "h")

                      # Sample size
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$text, "Sample size = 100")

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
