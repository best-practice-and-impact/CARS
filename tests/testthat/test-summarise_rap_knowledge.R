
dummy_data <- data.frame(heard_of_RAP = c(NA, "Yes", "No"))

test_that("summarise_rap_knowledge missing data is handled correctly", {

  got <- summarise_rap_knowledge(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_knowledge output is as expected", {

  got <- summarise_rap_knowledge(dummy_data)

  expected <- data.frame(

    value = factor(c(
      "Yes",
      "No"),
      levels = c(
        "Yes",
        "No")),

    n = rep(1/2, times = 2)

  )

  expect_equal(got, expected)

})
