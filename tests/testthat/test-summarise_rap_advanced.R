
dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 4), rep("regularly", 2), rep("All the time", 6)),
                         advanced_rap_score = c(0, 1, 2, 2, 3, 3, 3, 4, 4, 5, 6, 7, 7))

test_that("summarise_rap_advanced missing data is handled correctly", {

  dummy_data[3,1] <- NA

  got <- summarise_rap_advanced(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_advanced output is as expected", {

  got <- summarise_rap_advanced(dummy_data)

  expected <- data.frame(value = factor(0:7,
                                        levels = c(0:7)),
                         n = c(0, 1/12, 1/6, 1/4, 1/6, 1/12, 1/12, 1/6))

  expect_equal(got, expected)

})
