dummy_data <- data.frame(reproducible_workflow = c(NA,
                                                   rep("Yes", 2),
                                                   rep("No", 3),
                                                   rep("I don't know what reproducible workflows are", 4)))

test_that("summarise_rep_workflow missing data is handled correctly", {

  got <- summarise_rep_workflow(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rep_workflow output is as expected", {

  got <- summarise_rep_workflow(dummy_data)

  expected <- data.frame(value = factor(c("Yes",
                                          "No",
                                          "I don't know what reproducible workflows are"),
                                        levels = c("Yes",
                                                   "No",
                                                   "I don't know what reproducible workflows are")),
                         n = c(2/9, 1/3, 4/9))

  expect_equal(got, expected)

})


test_that("summarise_rep_workflow validation works", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_rep_workflow(dummy_data), "unexpected_input: no column called 'reproducible_workflow'")

})
