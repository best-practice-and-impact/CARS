
dummy_data <- data.frame(heard_of_RAP = c("No", rep("Yes", 13)),
                         know_RAP_champ = c(rep("I don't know what a RAP champion is", 2),
                                            rep("I know what a RAP champion is but don't know who the RAP champion in my department is", 3),
                                            rep("I know what a RAP champion is and there is no RAP champion in my department", 4),
                                            rep("I know who the RAP champion in my department is", 5))
)

test_that("summarise_rap_knowledge missing data is handled correctly", {

  got <- summarise_rap_knowledge(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_knowledge output is as expected", {

  got <- summarise_rap_knowledge(dummy_data)

  expected <- data.frame(value = factor(c("Have not heard of RAP",
                                          "I don't know what a RAP champion is",
                                          "I know what a RAP champion is but don't know who the RAP champion in my department is",
                                          "I know what a RAP champion is and there is no RAP champion in my department",
                                          "I know who the RAP champion in my department is"),
                                        levels = c("Have not heard of RAP",
                                                   "I don't know what a RAP champion is",
                                                   "I know what a RAP champion is but don't know who the RAP champion in my department is",
                                                   "I know what a RAP champion is and there is no RAP champion in my department",
                                                   "I know who the RAP champion in my department is")),
                         n = c(0.07, 0.07, 0.21, 0.29, 0.36))

  expect_equal(got, expected)

})

test_that("summarise_rap_knowledge validation works", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_rap_knowledge(dummy_data), "unexpected_input: no column called 'heard_of_RAP'")

})
