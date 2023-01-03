# Coding frequency table
# Frequency table should not include missing values and should include counts of 0

test_that("summarise_code_freq works", {

  dummy_data <- data.frame(code_freq = c(rep("Sometimes", 3), rep("All the time", 2), "Never", NA))

  got <- summarise_code_freq(dummy_data)

  expect_false(any(is.na.data.frame(got)))

  expected <- data.frame(name = c(rep("Coding frequency", 5)),
                         value = factor(c("Never",
                                          "Rarely",
                                          "Sometimes",
                                          "Regularly",
                                          "All the time"),
                                          levels = c("Never",
                                                     "Rarely",
                                                     "Sometimes",
                                                     "Regularly",
                                                     "All the time")),
                         n=c(0.17, 0, 0.5, 0, 0.33))

  expect_equal(got, expected)
})

test_that("Validation checks work", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_code_freq(dummy_data), "unexpected_input: no column called 'code_freq'")

})
