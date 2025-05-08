

test_that("expected outputs achieved", {

  dummy_data <- data.frame(Q1 = rep(c("test1",
                                      "test2",
                                      "test3"), each = 3),
                           Q2 = factor(rep(c(1, 2, 3), 3),
                                       levels = c(1, 2, 3)),
                           n = c(0.1, 0.3, 0.6, 0.5, 0.25,
                                 0.25, 0.33, 0.33, 0.34),
                           count = c(10, 30, 60, 50, 25, 25, 33, 33, 34),
                           sample = c(100, 100, 100, 100, 100, 100, 100, 100, 100))

  testthat::expect_equal(calculate_bases(dummy_data, mid = 2, neutral_mid = TRUE),
                         c(-0.250, -0.150, 0.150, -0.625, -0.125,
                           0.125, -0.495, -0.165, 0.165))

})


test_that("expected outputs achieved", {

  dummy_data <- data.frame(Q1 = factor(c("test1", "test2", "test3"),
                                       levels = c("test1", "test2", "test3")),
                           n = c(0.5, 0.2, 0.8),
                           count = c(5, 2, 8),
                           sample = c(10, 10, 10))

  got <- plot_freqs(dummy_data, n = 100, xlab = "x", ylab = "y")

  axis_got <- set_axis_range(got, min = 0.1, max = 0.8, axis = "y")

  testthat::expect_equal(axis_got$x$layoutAttrs[[3]]$yaxis$range[[1]], 0.1)
  testthat::expect_equal(axis_got$x$layoutAttrs[[3]]$yaxis$range[[2]], 0.8)

})


test_that("expected outputs achieved", {
  testthat::expect_equal(create_y_lab("y", 12),
                         list(text = "y",
                              y = 1,
                              x = "min",
                              showarrow = FALSE,
                              yshift = 24,
                              xref = "paper",
                              yref = "paper",
                              font = list(size = 12 * 1.2)))

})


test_that("expected outputs achieved", {
  testthat::expect_equal(axis_settings("x", "y", 12),
                         list(
                           scale_axis = list(
                             title = "y",
                             tickfont = list(size = 12),
                             titlefont = list(size = 12 * 1.2),
                             tickformat = ".0%",
                             title = "Percent"
                             ),
                           cat_axis = list(
                             title = "x",
                             tickfont = list(size = 12),
                             titlefont = list(size = 12 * 1.2)
                             )
                           )
                         )

})
