dummy_data <- data.frame(coding_ability_change = c(NA,
                                                   rep("Significantly worse", 2),
                                                   rep("Slightly worse", 3),
                                                   rep("No change", 4),
                                                   rep("Slightly better", 5),
                                                   rep("Significantly better", 6)))

dummy_output <- summarise_ability_change(dummy_data)

test_that("Check output is dataframe" , {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output does not contain missing values", {
  expect_false(any(is.na(dummy_output)))
})

test_that("Output has five rows", {
  expect_equal(nrow(dummy_output), 5)
})

test_that("Output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("Output column names are correct", {
  expect_equal(colnames(dummy_output), c("name", "value", "n"))
})

test_that("names are in the correct order", {
  expect_identical(unique(dummy_output[[1]]), "Ability Change")
})

test_that("labels are in the correct order", {
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
  expect_equal(dummy_output$n, c(2, 3, 4, 5, 6))
})
