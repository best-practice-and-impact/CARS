
dummy_data <- data.frame(heard_of_RAP = c("No", rep("Yes", 13)),
                         know_RAP_champ = c(rep("I don't know what a RAP champion is", 2),
                                            rep("I know what a RAP champion is but don't know who the RAP champion in my department is", 3),
                                            rep("I know what a RAP champion is and there is no RAP champion in my department", 4),
                                            rep("I know who the RAP champion in my department is", 5)
                         ))

dummy_output <- summarise_rap_knowledge(dummy_data)

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
  expect_identical(unique(dummy_output[[1]]), "RAP champion knowledge")
})

test_that("Labels are in correct order",{
  expect_identical(unique(dummy_output[[2]]),
                   factor(c("Have not heard of RAP",
                            "I don't know what a RAP champion is",
                            "I know what a RAP champion is but don't know who the RAP champion in my department is",
                            "I know what a RAP champion is and there is no RAP champion in my department",
                            "I know who the RAP champion in my department is"),
                          levels = c("Have not heard of RAP",
                                     "I don't know what a RAP champion is",
                                     "I know what a RAP champion is but don't know who the RAP champion in my department is",
                                     "I know what a RAP champion is and there is no RAP champion in my department",
                                     "I know who the RAP champion in my department is")))
})


test_that("Check output values are correct",{

  expect_equal(dummy_output$n, c(1, 1, 3, 4, 5))

})
