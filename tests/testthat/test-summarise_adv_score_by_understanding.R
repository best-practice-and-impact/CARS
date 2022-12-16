test_that("Outcome is as expected", {

  dummy_data <- data.frame(RAP_components = c(NA,
                                              rep("Strongly disagree", 2),
                                              rep("Disagree", 3),
                                              rep("Neutral", 4),
                                              rep("Agree", 7),
                                              rep("Strongly agree", 5)),
                           advanced_rap_score = c(NA,
                                                  0,
                                                  rep(1, 2),
                                                  rep(2, 3),
                                                  rep(3, 4),
                                                  rep(4, 5),
                                                  rep(5, 3),
                                                  rep(6, 2),
                                                  7))

  got <- summarise_adv_score_by_understanding(dummy_data)

  expected <- data.frame(RAP_components = factor(c(rep("Strongly disagree", 8),
                                                   rep("Disagree", 8),
                                                   rep("Neutral", 8),
                                                   rep("Agree", 8),
                                                   rep("Strongly agree", 8)),
                                                 levels = c("Strongly disagree",
                                                            "Disagree",
                                                            "Neutral",
                                                            "Agree",
                                                            "Strongly agree")),
                         advanced_rap_score = factor(rep(c(0,1,2,3,4,5,6,7), 5),
                                                     levels = c(0,1,2,3,4,5,6,7)),
                         n = c(1, 1, 0, 0, 0, 0, 0, 0,
                               0, 1, 2, 0, 0, 0, 0, 0,
                               0, 0, 1, 3, 0, 0, 0, 0,
                               0, 0, 0, 1, 5, 1, 0, 0,
                               0, 0, 0, 0, 0, 2, 2, 1))

  expect_equal(got, expected)

})