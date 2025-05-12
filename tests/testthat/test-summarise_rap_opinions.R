
dummy_data <- data.frame(rap_important = c("Neutral", "Disagree", "Disagree", "Strongly Agree" , NA),
                         rap_resources = c("Strongly Disagree", "Strongly Agree" , "Agree", "Neutral", "Disagree"),
                         rap_implementing = c("Disagree", "Strongly Disagree", "Strongly Agree" , "Agree", "Neutral"),
                         dept_values_rap =  c("Strongly Agree" , "Agree", "Neutral", "Disagree", "Strongly Disagree"),
                         rap_other = c("text" , "text", NA, "text", "text" ))

test_that("summarise_rap_opinions missing data is handled correctly", {

  got <- summarise_rap_opinions(dummy_data, config, question = "rap_opinions")

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_opinions output is as expected", {

  got <- summarise_rap_opinions(dummy_data, config, question = "rap_opinions")

  expected <- data.frame(name = factor(c(rep("I think it is important to implement RAP in my work", 5),
                                  rep("I have the resources I need to help me implement RAP in my work", 5),
                                  rep("I am currently implementing RAP in my work", 5),
                                  rep("My department values RAP", 5)),
                                  levels = c("I think it is important to implement RAP in my work",
                                             "I have the resources I need to help me implement RAP in my work",
                                             "I am currently implementing RAP in my work",
                                             "My department values RAP",
                                             "Other")),
                         value = factor(c("Strongly Disagree",
                                          "Disagree",
                                          "Neutral",
                                          "Agree",
                                          "Strongly Agree"),
                                        levels = c("Strongly Disagree",
                                                   "Disagree",
                                                   "Neutral",
                                                   "Agree",
                                                   "Strongly Agree")),
                         n = c(0, 2/4, 1/4, 0, 1/4, rep(1/5, 15)),
                         count = c(0, 2, 1, 0, 1, rep(1, 15)),
                         sample = c(rep(4, 5), rep(5, 15))
  )

  expect_equal(got, expected)

})

test_that("summarise_rap_opinions validation works", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_rap_opinions(dummy_data, config, question = "rap_opinions"), "unexpected_input: check column names")

})
