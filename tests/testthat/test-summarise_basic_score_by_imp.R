test_that("Outcome is as expected", {

  dummy_data <- data.frame(RAP_implementing = c(NA,
                                                rep("Strongly disagree", 2),
                                                rep("Disagree", 3),
                                                rep("Neutral", 4),
                                                rep("Agree", 5),
                                                rep("Strongly agree", 3)),
                           basic_rap_score = c(NA,
                                               0,
                                               rep(1, 2),
                                               rep(2, 3),
                                               rep(3, 4),
                                               rep(4, 4),
                                               rep(5, 2),
                                               6))

  got <- summarise_basic_score_by_imp(dummy_data)

  expected <- data.frame(RAP_implementing = factor(c(rep("Strongly disagree", 7),
                                                     rep("Disagree", 7),
                                                     rep("Neutral", 7),
                                                     rep("Agree", 7),
                                                     rep("Strongly agree", 7)),
                                                   levels = c("Strongly disagree",
                                                              "Disagree",
                                                              "Neutral",
                                                              "Agree",
                                                              "Strongly agree")),
                         basic_rap_score = factor(rep(c(0,1,2,3,4,5,6), 5),
                                                  levels = c(0,1,2,3,4,5,6)),
                         n = c(1, 1, 0, 0, 0, 0, 0,
                               0, 1, 2, 0, 0, 0, 0,
                               0, 0, 1, 3, 0, 0, 0,
                               0, 0, 0, 1, 4, 0, 0,
                               0, 0, 0, 0, 0, 2, 1))

  expect_equal(got, expected)

})
