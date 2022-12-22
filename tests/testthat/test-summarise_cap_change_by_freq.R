dummy_data <- data.frame(coding_ability_change = c(NA,
                                                   rep("Significantly worse", 2),
                                                   rep("Slightly worse", 3),
                                                   rep("No change", 4),
                                                   rep("Slightly better", 5),
                                                   rep("Significantly better", 6)),
                         code_freq = c(NA,
                                       rep("Sometimes", 6),
                                       rep("All the time", 5),
                                       rep("Never", 3),
                                       rep("Rarely", 4),
                                       rep("Regularly", 2)))

dummy_output <- summarise_cap_change_by_freq(dummy_data)

test_that("Check output is dataframe" , {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output does not contain missing values", {
  expect_false(any(is.na(dummy_output)))
})

test_that("Output has twenty-five rows", {
  expect_equal(nrow(dummy_output), 25)
})

test_that("Output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("Output column names are correct", {
  expect_equal(colnames(dummy_output), c("code_freq", "coding_ability_change", "n"))
})

test_that("labels for first column are in the correct order", {
  expect_identical(unique(dummy_output[[1]]),
                   factor(c("Never",
                            "Rarely",
                            "Sometimes",
                            "Regularly",
                            "All the time"),
                          levels = c("Never",
                                     "Rarely",
                                     "Sometimes",
                                     "Regularly",
                                     "All the time"))
  )
})

test_that("labels for second column are in the correct order", {
  expect_identical(unique(dummy_output[[2]]),
                   factor(c("Significantly worse",
                            "Slightly worse",
                            "No change",
                            "Slightly better",
                            "Significantly better"),
                          levels = c("Significantly worse",
                                     "Slightly worse",
                                     "No change",
                                     "Slightly better",
                                     "Significantly better"))
  )
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[dummy_output["coding_ability_change"] == "Significantly worse",]$n, c(0, 0, 2, 0, 0))
  expect_equal(dummy_output[dummy_output["coding_ability_change"] == "Slightly worse",]$n, c(0, 0, 3, 0, 0))
  expect_equal(dummy_output[dummy_output["coding_ability_change"] == "No change",]$n, c(0, 0, 1, 0, 3))
  expect_equal(dummy_output[dummy_output["coding_ability_change"] == "Slightly better",]$n, c(3, 0, 0, 0, 2))
  expect_equal(dummy_output[dummy_output["coding_ability_change"] == "Significantly better",]$n, c(0, 4, 0, 2, 0))
})
