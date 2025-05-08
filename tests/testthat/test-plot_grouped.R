levels1 <- c("This is test number one", "This is test 2", "test3", "test4", "test5")

dummy_data <- data.frame(Q1 = factor(levels1,
                                    levels = levels1),
                         Q2 = c(1, 1, 1, 2, 2),
                         n = c(0.2, 0.5, 0.3, 0.8, 0.6),
                         count = c(2, 5, 3, 8, 6),
                         sample = c(10, 10, 10, 10, 10))


test_that("validity checks work",
                    {
                      testthat::expect_error(plot_grouped(as.list(dummy_data)), "Unexpected input - data is not a data.frame.")
                      testthat::expect_error(plot_grouped(dplyr::mutate(dummy_data, Q3 = Q1)), "Unexpected input - data does not contain 5 columns.")
                      testthat::expect_error(plot_grouped(dplyr::mutate(dummy_data, n = as.character(n))), "Unexpected input - data column 3 is not numeric.")
                      testthat::expect_error(plot_grouped(dummy_data, xlab = 1), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_grouped(dummy_data, ylab = 1), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_grouped(dummy_data, xlab = c("1", "2")), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_grouped(dummy_data, ylab = c("1", "2")), "Unexpected input - labels should be single character strings.")
                      testthat::expect_error(plot_grouped(dummy_data, font_size = "x"), "Unexpected input - font_size is not numeric.")
                    })

test_that("expected outputs for vertical chart achieved",
                    {

                      got <- plot_grouped(dummy_data, xlab = "x", ylab = "y",
                                          break_q_names_col = TRUE,
                                          orientation = "v")

                      # x and y values
                      testthat::expect_equal(c(got$x$attrs[[1]]$x), factor(c("This is test<br>number one", "This is test<br>2", "test3", "test4", "test5"),
                                                                           levels = c("This is test<br>number one", "This is test<br>2", "test3", "test4", "test5")))
                      testthat::expect_equal(c(got$x$attrs[[1]]$y), c(0.2, 0.5, 0.3, 0.8, 0.6))

                      # Bar colors
                      testthat::expect_equal(got$x$attrs[[1]]$marker$color, c("#F46A25", "#F46A25", "#F46A25", "#12436D", "#12436D"))

                      # Sample size
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$text, "")

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

                      got <- plot_grouped(dummy_data, xlab = "x", ylab = "y",
                                          break_q_names_col = TRUE,
                                          orientation = "h")

                      # x and y values
                      testthat::expect_equal(c(got$x$attrs[[1]]$y), factor(rev(c("This is test<br>number one", "This is test<br>2", "test3", "test4", "test5")),
                                                                           levels = rev(c("This is test<br>number one", "This is test<br>2", "test3", "test4", "test5"))))
                      testthat::expect_equal(c(got$x$attrs[[1]]$x), rev(c(0.2, 0.5, 0.3, 0.8, 0.6)))

                      # Bar colors
                      testthat::expect_equal(got$x$attrs[[1]]$marker$color, rev(c("#F46A25", "#F46A25", "#F46A25", "#12436D", "#12436D")))

                      # Sample size
                      testthat::expect_equal(got$x$layoutAttrs[[1]]$annotations$text, "")

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
