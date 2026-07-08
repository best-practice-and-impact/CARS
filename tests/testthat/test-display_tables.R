test_that("heatmap styling removes cell spacing", {
  dummy_data <- data.frame(
    Q1 = factor(c("test1", "test2"), levels = c("test1", "test2")),
    n = c(0.5, 0.2),
    count = c(5, 2),
    sample = c(10, 10)
  )

  got <- df_to_table(dummy_data, heatmap = TRUE, sample = FALSE)
  html <- as.character(got)

  testthat::expect_match(html, "padding:\\s*6px\\s*10px\\s*!important")
  testthat::expect_true(grepl("border-bottom:\\s*0\\s*!important", html))
  testthat::expect_true(grepl("border-collapse:\\s*collapse", html))
  testthat::expect_true(grepl("border-spacing:\\s*0", html))
  testthat::expect_false(grepl("-4.1px", html, fixed = TRUE))
})

test_that("df_to_table works without heatmap", {
  dummy_data <- data.frame(
    Q1 = factor(c("test1", "test2"), levels = c("test1", "test2")),
    n = c(0.5, 0.2),
    count = c(5, 2),
    sample = c(10, 10)
  )

  got <- df_to_table(dummy_data, heatmap = FALSE, sample = FALSE)
  html <- as.character(got)

  testthat::expect_true(is.character(html))
  testthat::expect_true(nchar(html) > 0)
  testthat::expect_true(grepl("<table>", html))
})

