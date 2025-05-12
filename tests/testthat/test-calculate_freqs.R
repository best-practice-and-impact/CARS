
dummy_data <- data.frame(Q1 = c("test1", NA),
                         Q2 = c("test2", "test2"),
                         Q3 = c("test1", "test3"))

cols <- c("Q1", "Q2", "Q3")

levels <- c("test1", "test2", "test3")

dummy_data[] <- lapply(dummy_data, factor, levels = levels)

labels <- list(Q1 = "Question 1",
               Q2 = "Question 2",
               Q3 = "Question 3")

expected <- data.frame(name = rep(c("Question 1",
                                    "Question 2",
                                    "Question 3"),
                                  each=3),
                       value = factor(rep(c("test1", "test2", "test3"), 3),
                                      levels = c("test1", "test2", "test3")),
                       n = c(1, 0, 0, 0, 1, 0, 0.5, 0, 0.5))

test_that("create_tidy_freq_table validation works", {

  expect_error(calculate_freqs(data = dummy_data,
                               cols = cols,
                               labels = NULL),
               "Missing input: labels needed for mutli-column frequencies.")

})

test_that("calculate_freqs missing data is handled correctly", {

  got <- calculate_freqs(data = dummy_data,
                         cols = cols,
                         labels = labels)

  expect_false(any(is.na.data.frame(got)))

})

test_that("calculate_freqs proportion output is as expected", {

  got <- calculate_freqs(data = dummy_data,
                         cols = cols,
                         labels = labels)

  expect_equal(got, expected)

})

test_that("calculate_freqs count output is as expected", {

  got <- calculate_freqs(data = dummy_data,
                         cols = cols,
                         labels = labels,
                         prop = FALSE)

  expected$n <- c(1, 0, 0, 0, 2, 0, 1, 0, 1)

  expect_equal(got, expected)

})

test_that("calculate_freqs sample output is as expected", {

  got <- calculate_freqs(data = dummy_data,
                         cols = cols,
                         labels = labels,
                         prop = FALSE,
                         sample = TRUE)

  expected$n <- c(1, 0, 0, 0, 2, 0, 1, 0, 1)
  expected$sample <- c(1, 1, 1, 2, 2, 2, 2, 2, 2)

  expect_equal(got, expected)

})


test_that("calculate_freqs single question proportion output is as expected", {

  cols <- "Q1"

  got <- calculate_freqs(data = dummy_data,
                         cols = cols,
                         labels = labels)

  expected <- data.frame(value = factor(c("test1", "test2", "test3"),
                                        levels = c("test1", "test2", "test3")),
                         n = c(1, 0, 0))

  expect_equal(got, expected)

})

test_that("calculate_freqs single question count output is as expected", {

  cols <- "Q1"

  got <- calculate_freqs(data = dummy_data,
                         cols = cols,
                         labels = labels,
                         prop = FALSE)

  expected <- data.frame(value = factor(c("test1", "test2", "test3"),
                                        levels = c("test1", "test2", "test3")),
                         n = c(1, 0, 0))

  expect_equal(got, expected)

})

test_that("calculate_freqs single question sample output is as expected", {

  cols <- "Q1"

  got <- calculate_freqs(data = dummy_data,
                         cols = cols,
                         labels = labels,
                         prop = FALSE,
                         sample = TRUE)

  expected <- data.frame(value = factor(c("test1", "test2", "test3"),
                                        levels = c("test1", "test2", "test3")),
                         n = c(1, 0, 0),
                         sample = c(1, 1, 1))

  expect_equal(got, expected)

})


