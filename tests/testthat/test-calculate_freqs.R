
dummy_data <- data.frame(Q1 = c("test1", NA),
                         Q2 = c("test2", "test2"),
                         Q3 = c("test1", "test3"))

questions <- c("Q1", "Q2", "Q3")

levels <- c("test1", "test2", "test3")

labels <- c("Question 1", "Question 2", "Question 3")

#TODO: test proportion outputs as well as raw counts

dummy_output <- calculate_freqs(data = dummy_data,
                                questions = questions,
                                levels = levels,
                                labels = labels,
                                prop = FALSE)

test_that("output is a dataframe", {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("output does not contain missing values", {
  expect_false(any(is.na.data.frame(dummy_output)))
})

test_that("names are in the correct order", {
  expect_identical(unique(dummy_output[[1]]),
                   c("Question 1", "Question 2", "Question 3")
  )
})

test_that("values are in the correct order", {
  expect_identical(unique(dummy_output[[2]]),
                   factor(c("test1", "test2", "test3"),
                          levels = c("test1", "test2", "test3"))
  )
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[[3]], c(1, 0, 0, 0, 2, 0, 1, 0, 1))
})
