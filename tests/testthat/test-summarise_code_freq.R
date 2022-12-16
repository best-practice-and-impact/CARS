# Coding frequency table
# Frequency table should not include missing values and should include counts of 0

test_that("Output is as expected", {

  dummy_data <- data.frame(code_freq = c(rep("Sometimes", 3), rep("All the time", 2), "Never", NA))

  got <- summarise_code_freq(dummy_data)

  expected <- data.frame("name" = c(rep("Coding frequency", 5)),
                         "value" = factor(c("Never",
                                            "Rarely",
                                            "Sometimes",
                                            "Regularly",
                                            "All the time"),
                                          levels = c("Never",
                                                     "Rarely",
                                                     "Sometimes",
                                                     "Regularly",
                                                     "All the time")),
                         "n"=c(1, 0, 3, 0, 2))

  expect_equal(got, expected)
})
