
dummy_data <- data.frame(

  heard_of_RAP = rep(c(
    NA,
    "No",
    "Yes"),
    each = 4),

  strategy_knowledge = rep(c(
    NA,
    "Yes",
    "Yes, but I haven't read it",
    "No"),
    times = 3)

)

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

  expected <- data.frame(

    value = factor(c(
      "Yes",
      "Yes, but I haven't read it",
      "No"),
      levels = c(
        "Yes",
        "Yes, but I haven't read it",
        "No")),

    n = rep(1/3, times = 3)

  )

  expect_equal(got, expected)

})
