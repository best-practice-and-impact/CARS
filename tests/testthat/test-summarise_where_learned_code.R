dummy_data <- data.frame(code_freq = c(rep("Never", 8), "Sometimes", "Regularly"),
                         prev_coding_experience = c(rep("Yes", 8), NA, "No"),
                         first_learned = c(rep("Self-taught" , 3),
                                           rep( "In public sector employment", 3),
                                           rep("other" , 1),
                                           rep(NA , 3)))

dummy_output <- summarise_where_learned_code(dummy_data)

test_that("Check output is dataframe" , {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output does not contain missing values", {
  expect_false(any(is.na(dummy_output)))
})

test_that("Output has six rows", {
  expect_equal(nrow(dummy_output), 6)
})

test_that("Output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("Output column names are correct", {
  expect_equal(colnames(dummy_output), c("name", "value", "n"))
})

test_that("names are in the correct order", {
  expect_identical(unique(dummy_output[[1]]), "First coding experience")
})

test_that("values are in correct order",{
  expect_identical(unique(dummy_output[[2]]),
                   factor(c("In current role",
                            "In education",
                            "In private sector employment",
                            "In public sector employment",
                            "Self-taught",
                            "Other"),
                          levels = c("In current role",
                                     "In education",
                                     "In private sector employment",
                                     "In public sector employment",
                                     "Self-taught",
                                     "Other"))
  )
})


test_that("Values in output are correct", {
  expect_equal(dummy_output$n, c(2, 0, 0, 3, 3, 1))
})
