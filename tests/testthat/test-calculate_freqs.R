
dummy_data <- data.frame(Q1 = c("test1", NA),
                         Q2 = c("test2", "test2"),
                         Q3 = c("test1", "test3"))

questions <- c("Q1", "Q2", "Q3")

levels <- c("test1", "test2", "test3")

labels <- c("Question 1", "Question 2", "Question 3")

expected <- data.frame(name = rep(c("Question 1",
                                    "Question 2",
                                    "Question 3"),
                                  each=3),
                       value = factor(rep(c("test1", "test2", "test3"), 3),
                                      levels = c("test1", "test2", "test3")),
                       n = c(1, 0, 0, 0, 1, 0, 0.5, 0, 0.5))

test_that("create_tidy_freq_table validation works", {

  expect_error(calculate_freqs(data = dummy_data,
                               questions = questions,
                               levels = levels),
               "Missing input: labels needed for mutli-column frequencies.")

})

test_that("create_tidy_freq_table missing data is handled correctly", {

  got <- calculate_freqs(data = dummy_data,
                         questions = questions,
                         levels = levels,
                         labels = labels)

  expect_false(any(is.na.data.frame(got)))

})

test_that("create_tidy_freq_table proportion output is as expected", {

  got <- calculate_freqs(data = dummy_data,
                         questions = questions,
                         levels = levels,
                         labels = labels)

  expect_equal(got, expected)

})

test_that("create_tidy_freq_table count output is as expected", {

  got <- calculate_freqs(data = dummy_data,
                         questions = questions,
                         levels = levels,
                         labels = labels,
                         prop = FALSE)

  expected$n <- c(1, 0, 0, 0, 2, 0, 1, 0, 1)

  expect_equal(got, expected)

})

test_that("create_tidy_freq_table single question proportion output is as expected", {

  questions <- "Q1"

  got <- calculate_freqs(data = dummy_data,
                         questions = questions,
                         levels = levels)

  expected <- data.frame(value = factor(c("test1", "test2", "test3"),
                                        levels = c("test1", "test2", "test3")),
                         n = c(1, 0, 0))

  expect_equal(got, expected)

})

test_that("create_tidy_freq_table single question count output is as expected", {

  questions <- "Q1"

  got <- calculate_freqs(data = dummy_data,
                         questions = questions,
                         levels = levels,
                         prop = FALSE)

  expected <- data.frame(value = factor(c("test1", "test2", "test3"),
                                        levels = c("test1", "test2", "test3")),
                         n = c(1, 0, 0))

  expect_equal(got, expected)

})
