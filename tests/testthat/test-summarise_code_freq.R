# Coding frequency table
# Frequency table should not include missing values and should include counts of 0
#
# test_that("Output is as expected", {
#
#   dummy_data <- data.frame(code_freq = c(rep("Sometimes", 3), rep("All the time", 2), "Never", NA))
#
#   got <- summarise_code_freq(dummy_data)
#
#   expected <- data.frame(value = factor(c("Never",
#                                           "Rarely",
#                                           "Sometimes",
#                                           "Regularly",
#                                           "All the time"),
#                                           levels = c("Never",
#                                                      "Rarely",
#                                                      "Sometimes",
#                                                      "Regularly",
#                                                      "All the time")),
#                          n=c(1, 0, 3, 0, 2))
#
#   expected$n <- expected$n / nrow(dummy_data)
#
#   expect_equal(got, expected, tolerance = .1)
# })

test_that("Validation checks work", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_code_freq(dummy_data), "unexpected_input: no column called 'code_freq'")

})
