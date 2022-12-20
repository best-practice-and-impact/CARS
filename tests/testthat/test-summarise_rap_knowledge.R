
test_that("Output is as expected", {

  dummy_data <- data.frame(heard_of_RAP = c("No", rep("Yes", 13)),
                           know_RAP_champ = c(rep("I don't know what a RAP champion is", 2),
                                              rep("I know what a RAP champion is but don't know who the RAP champion in my department is", 3),
                                              rep("I know what a RAP champion is and there is no RAP champion in my department", 4),
                                              rep("I know who the RAP champion in my department is", 5)
                           ))

  got <- summarise_rap_knowledge(dummy_data)

  expected <- data.frame(name = c(rep("RAP champion knowledge", 5)),
                         value = factor(c("Have not heard of RAP",
                                          "I don't know what a RAP champion is",
                                          "I know what a RAP champion is but don't know who the RAP champion in my department is",
                                          "I know what a RAP champion is and there is no RAP champion in my department",
                                          "I know who the RAP champion in my department is"),
                                        levels = c("Have not heard of RAP",
                                                   "I don't know what a RAP champion is",
                                                   "I know what a RAP champion is but don't know who the RAP champion in my department is",
                                                   "I know what a RAP champion is and there is no RAP champion in my department",
                                                   "I know who the RAP champion in my department is")),
                         n = c(1, 1, 3, 4, 5))

  expect_equal(got, expected)

})

test_that("Validation checks work", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_rap_knowledge(dummy_data), "unexpected_input: no column called 'heard_of_RAP'")

})
