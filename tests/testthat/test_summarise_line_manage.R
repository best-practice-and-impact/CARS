dummy_data <- data.frame(management = c(NA,
                                        rep("Yes", 2),
                                        rep("No - I manage people who do not write code", 3),
                                        rep("No - I don't line manage anyone", 4)))

dummy_output <- summarise_line_manage(dummy_data)

test_that("output is a dataframe", {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output has two columns", {
  expect_equal(ncol(dummy_output), 2)
})

test_that("output does not contain missing values", {
  expect_false(any(is.na.data.frame(dummy_output)))
})

test_that("labels are in the correct order", {
  expect_identical(dummy_output[[1]],
                   factor(c("Yes",
                            "No - I manage people who do not write code",
                            "No - I don't line manage anyone"),
                          levels = c("Yes",
                                     "No - I manage people who do not write code",
                                     "No - I don't line manage anyone"))
  )
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[[2]], c(2, 3, 4))
})
