
test_that("create_tidy_freq_table works", {

  dummy_data <- data.frame(Q1 = c("test1", NA),
                           Q2 = c("test2", "test2"),
                           Q3 = c("test1", "test3"))

  questions <- c("Q1", "Q2", "Q3")

  levels <- c("test1", "test2", "test3")

  labels <- c("Question 1", "Question 2", "Question 3")

  got <- create_tidy_freq_table(data = dummy_data,
                                questions = questions,
                                levels = levels,
                                labels = labels)

  expect_false(any(is.na.data.frame(got)))

  expected <- data.frame(name = c(rep("Question 1", 3),
                                  rep("Question 2", 3),
                                  rep("Question 3", 3)),
                         value = factor(rep(c("test1", "test2", "test3"), 3),
                                        levels = c("test1", "test2", "test3")),
                         n = c(1, 0, 0, 0, 1, 0, 0.5, 0, 0.5))

  expect_equal(got, expected)

})
