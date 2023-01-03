test_that("summarise_adv_score_by_imp works", {

  dummy_data <- data.frame(RAP_implementing = c(NA,
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

  got <- summarise_adv_score_by_imp(dummy_data)

  expect_false(any(is.na.data.frame(got)))

  expected <- data.frame(RAP_implementing = factor(c(rep("Strongly disagree", 8),
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
                         n = c(0.50, 0.50, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                               0.00, 0.33, 0.67, 0.00, 0.00, 0.00, 0.00, 0.00,
                               0.00, 0.00, 0.25, 0.75, 0.00, 0.00, 0.00, 0.00,
                               0.00, 0.00, 0.00, 0.14, 0.71, 0.14, 0.00, 0.00,
                               0.00, 0.00, 0.00, 0.00, 0.00, 0.40, 0.40, 0.20))

  expect_equal(got, expected)

})
