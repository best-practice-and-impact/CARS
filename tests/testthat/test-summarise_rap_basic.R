
dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 4), rep("All the time", 6)),
                         basic_rap_score = c(0, 1, 2, 2, 3, 3, 3, 4, 4, 5, 6))

test_that("summarise_rap_basic missing data is handled correctly", {

  dummy_data[3,1] <- NA

  got <- summarise_rap_basic(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_basic output is as expected", {

  got <- summarise_rap_basic(dummy_data)

  expected <- data.frame(value = factor(0:6,
                                        levels = c(0:6)),
                         n = c(0.00, 0.10, 0.20, 0.30, 0.20, 0.10, 0.10))

  expect_equal(got, expected)

})
