
dummy_data <- data.frame(RAP_components = c(NA,
                                            rep("Strongly Disagree", 2),
                                            rep("Disagree", 3),
                                            rep("Neutral", 4),
                                            rep("Agree", 7),
                                            rep("Strongly Agree", 5)),
                         advanced_rap_score = c(NA,
                                                0,
                                                rep(1, 2),
                                                rep(2, 3),
                                                rep(3, 4),
                                                rep(4, 5),
                                                rep(5, 3),
                                                rep(6, 2),
                                                7))

test_that("summarise_adv_score_by_understanding missing data is handled correctly", {

  got <- summarise_adv_score_by_understanding(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_adv_score_by_understanding output is as expected", {

  got <- summarise_adv_score_by_understanding(dummy_data)

  expect_false(any(is.na.data.frame(got)))

  expected <- data.frame(RAP_components = factor(rep(c("Strongly Disagree",
                                                       "Disagree",
                                                       "Neutral",
                                                       "Agree",
                                                       "Strongly Agree"),
                                                     each=8),
                                                 levels = c("Strongly Disagree",
                                                            "Disagree",
                                                            "Neutral",
                                                            "Agree",
                                                            "Strongly Agree")),
                         advanced_rap_score = factor(rep(c(0,1,2,3,4,5,6,7), 5),
                                                     levels = c(0,1,2,3,4,5,6,7)),
                         n = c(0.50, 0.50, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                               0.00, 1/3, 2/3, 0.00, 0.00, 0.00, 0.00, 0.00,
                               0.00, 0.00, 0.25, 0.75, 0.00, 0.00, 0.00, 0.00,
                               0.00, 0.00, 0.00, 1/7, 5/7, 1/7, 0.00, 0.00,
                               0.00, 0.00, 0.00, 0.00, 0.00, 0.40, 0.40, 0.20))

  expect_equal(got, expected)

})
