dummy_data <- data.frame(reproducible_workflow = c(NA,
                                                   rep("Yes", 2),
                                                   rep("No", 3),
                                                   rep("I don't know what reproducible workflows are", 4)))

dummy_output <- summarise_rep_workflow(dummy_data)

test_that("Check output is dataframe" , {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output does not contain missing values", {
  expect_false(any(is.na(dummy_output)))
})

test_that("Output has three rows", {
  expect_equal(nrow(dummy_output), 3)
})

test_that("Output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("Output column names are correct", {
  expect_equal(colnames(dummy_output), c("name", "value", "n"))
})

test_that("names are in the correct order", {
  expect_identical(unique(dummy_output[[1]]), "Use reproducible workflow packages")
})

test_that("labels are in the correct order", {
  expect_identical(unique(dummy_output[[2]]),
                   factor(c("Yes",
                            "No",
                            "I don't know what reproducible workflows are"),
                          levels = c("Yes",
                                     "No",
                                     "I don't know what reproducible workflows are"))
  )
})

test_that("frequencies are correct", {
  expect_equal(dummy_output$n, c(2, 3, 4))
})
