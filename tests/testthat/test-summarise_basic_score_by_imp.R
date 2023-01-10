
dummy_data <- data.frame(RAP_implementing = c(NA,
                                              rep("Strongly Disagree", 2),
                                              rep("Disagree", 3),
                                              rep("Neutral", 4),
                                              rep("Agree", 5),
                                              rep("Strongly Agree", 3)),
                         basic_rap_score = c(NA,
                                             0,
                                             rep(0.50, 2),
                                             rep(2, 3),
                                             rep(3, 4),
                                             rep(4, 4),
                                             rep(5, 2),
                                             6))

test_that("summarise_basic_score_by_imp missing data is handled correctly", {

  got <- summarise_basic_score_by_imp(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_basic_score_by_imp output is as expected", {

  got <- summarise_basic_score_by_imp(dummy_data)

  expected <- data.frame(RAP_implementing = factor(rep(c("Strongly Disagree",
                                                         "Disagree",
                                                         "Neutral",
                                                         "Agree",
                                                         "Strongly Agree"),
                                                       each=7),
                                                   levels = c("Strongly Disagree",
                                                              "Disagree",
                                                              "Neutral",
                                                              "Agree",
                                                              "Strongly Agree")),
                         basic_rap_score = factor(rep(c(0,1,2,3,4,5,6), 5),
                                                  levels = c(0,1,2,3,4,5,6)),
                         n = c(1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                               0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.00,
                               0.00, 0.00, 0.25, 0.75, 0.00, 0.00, 0.00,
                               0.00, 0.00, 0.00, 0.20, 0.80, 0.00, 0.00,
                               0.00, 0.00, 0.00, 0.00, 0.00, 2/3, 1/3))

  expect_equal(got, expected)

})
