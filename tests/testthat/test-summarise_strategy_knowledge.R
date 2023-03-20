
dummy_data <- data.frame(heard_of_RAP = c("No", rep("Yes", 9)),
                         strategy_knowledge = c(NA,
                                                rep("I have not heard of the RAP strategy", 2),
                                                rep("I have heard of the RAP strategy, but I haven't read it", 3),
                                                rep("I have read the RAP strategy", 4)))

test_that("summarise_strategy_knowledge validation works", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_strategy_knowledge(dummy_data), "unexpected_input: no column called 'strategy_knowledge'")

  dummy_data <- data.frame(strategy_knowledge = c("test1", "test2"))

  expect_error(summarise_strategy_knowledge(dummy_data), "unexpected_input: no column called 'heard_of_RAP'")

})

test_that("summarise_strategy_knowledge missing data is handled correctly", {

  got <- summarise_strategy_knowledge(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_strategy_knowledge output is as expected", {

  got <- summarise_strategy_knowledge(dummy_data)

  expected <- data.frame(value = factor(c("I have not heard of the RAP strategy",
                                          "I have heard of the RAP strategy, but I haven't read it",
                                          "I have read the RAP strategy"),
                                        levels = c("I have not heard of the RAP strategy",
                                                   "I have heard of the RAP strategy, but I haven't read it",
                                                   "I have read the RAP strategy")),
                         n = c(2/9, 1/3, 4/9))

  expect_equal(got, expected)

})
