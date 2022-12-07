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

test_that("output is a dataframe", {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("output does not contain missing values", {
  expect_false(any(is.na.data.frame(dummy_output)))
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
  expect_true(all(subset(dummy_output, `Coding ability change`=="Significantly worse", select=Count) == c(0, 0, 2, 0, 0)))
  expect_true(all(subset(dummy_output, `Coding ability change`=="Slightly worse", select=Count) == c(0, 0, 3, 0, 0)))
  expect_true(all(subset(dummy_output, `Coding ability change`=="No change", select=Count) == c(0, 0, 1, 0, 3)))
  expect_true(all(subset(dummy_output, `Coding ability change`=="Slightly better", select=Count) == c(3, 0, 0, 0, 2)))
  expect_true(all(subset(dummy_output, `Coding ability change`=="Significantly better", select=Count) == c(0, 4, 0, 2, 0)))
})