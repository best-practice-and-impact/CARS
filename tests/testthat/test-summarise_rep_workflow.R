
test_that("Output is as expected", {

  dummy_data <- data.frame(reproducible_workflow = c(NA,
                                                     rep("Yes", 2),
                                                     rep("No", 3),
                                                     rep("I don't know what reproducible workflows are", 4)))

  got <- summarise_rep_workflow(dummy_data)

  expected <- data.frame(name = c(rep("Use reproducible workflow packages", 3)),
                         value = factor(c("Yes",
                                          "No",
                                          "I don't know what reproducible workflows are"),
                                        levels = c("Yes",
                                                   "No",
                                                   "I don't know what reproducible workflows are")),
                         n = c(2, 3, 4))

  expect_equal(got, expected)

})

test_that("Validation checks work", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_rep_workflow(dummy_data), "unexpected_input: no column called 'reproducible_workflow'")

})
