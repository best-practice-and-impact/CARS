
test_that("summarise_rap_opinions works", {

  dummy_data <- data.frame(heard_of_RAP = c("No", "Yes", "Yes", "Yes", "Yes"),
                           RAP_confident = c("Strongly Agree" , "Agree", "Neutral", "Disagree", "Strongly Disagree"),
                           RAP_supported = c("Strongly Disagree", "Strongly Agree" , "Agree", "Neutral", "Disagree"),
                           RAP_resources = c("Disagree", "Strongly Disagree", "Strongly Agree" , "Agree", "Neutral"),
                           RAP_components = c("Neutral", "Disagree", "Disagree", "Strongly Agree" , "Agree"),
                           RAP_important = c("Strongly Agree" , "Agree", NA, "Disagree", "Strongly Agree" ),
                           RAP_implementing = c("Strongly Agree" , "Agree", "Neutral", "Disagree", "Strongly Disagree"),
                           RAP_planning = c("Strongly Disagree", "Strongly Agree" , "Agree", "Neutral", "Disagree"))


  got <- summarise_rap_opinions(dummy_data)

  expect_false(any(is.na.data.frame(got)))

  expected <- data.frame(name = c(rep("I and/or my team are currently implementing RAP", 5),
                                  rep("I feel confident implementing RAP in my work", 5),
                                  rep("I feel supported to implement RAP in my work", 5),
                                  rep("I know where to find resources to help me implement RAP", 5),
                                  rep("I or my team are planning on implementing RAP in the next 12 months", 5),
                                  rep("I think it is important to implement RAP in my work", 5),
                                  rep("I understand what the key components of the RAP methodology are", 5)),
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
                         n = c(0.25, 0.25, 0.25, 0.25, 0.00, 0.25, 0.25,
                               0.25, 0.25, 0.00, 0.00, 0.25, 0.25, 0.25,
                               0.25, 0.25, 0.00, 0.25, 0.25, 0.25, 0.00,
                               0.25, 0.25, 0.25, 0.25, 0.00, 0.33, 0.00,
                               0.33, 0.33, 0.00, 0.50, 0.00, 0.25, 0.25))

  expect_equal(got, expected)

})

test_that("Validation checks work", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_rap_opinions(dummy_data), "unexpected_input: no column called 'heard_of_RAP'")

})
