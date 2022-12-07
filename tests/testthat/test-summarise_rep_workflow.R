dummy_data <- data.frame(reproducible_workflow = c(NA,
                                                   rep("Yes", 2),
                                                   rep("No", 3),
                                                   rep("I don't know what reproducible workflows are", 4)))

dummy_output <- summarise_rep_workflow(dummy_data)

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
                            "No",
                            "I don't know what reproducible workflows are"),
                          levels = c("Yes",
                                     "No",
                                     "I don't know what reproducible workflows are"))
  )
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[[2]], c(2, 3, 4))
})