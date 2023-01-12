# Coding frequency table
# Frequency table should not include missing values and should include counts of 0

dummy_data <- data.frame(code_freq = c(rep("Sometimes", 3), rep("All the time", 2), "Never", NA))

test_that("summarise_code_freq validation works", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_code_freq(dummy_data), "unexpected_input: no column called 'code_freq'")

})

test_that("summarise_code_freq missing data is handled correctly", {

  got <- summarise_code_freq(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_code_freq output is as expected", {

  got <- summarise_code_freq(dummy_data)

  expected <- data.frame(value = factor(c("Never",
                                          "Rarely",
                                          "Sometimes",
                                          "Regularly",
                                          "All the time"),
                                          levels = c("Never",
                                                     "Rarely",
                                                     "Sometimes",
                                                     "Regularly",
                                                     "All the time")),
                         n=c(1/6, 0, 0.5, 0, 1/3))

  expect_equal(got, expected)
})
