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

dummy_output <- summarise_basic_score_by_imp(dummy_data)

test_that("Check output is dataframe" , {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output does not contain missing values", {
  expect_false(any(is.na(dummy_output)))
})

test_that("Output has thirty-five rows", {
  expect_equal(nrow(dummy_output), 35)
})

test_that("Output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("Output column names are correct", {
  expect_equal(colnames(dummy_output), c("RAP_implementing", "basic_rap_score", "n"))
})

test_that("labels for first column are in the correct order", {
  expect_identical(unique(dummy_output[[1]]),
                   factor(c("Strongly disagree",
                            "Disagree",
                            "Neutral",
                            "Agree",
                            "Strongly agree"),
                          levels = c("Strongly disagree",
                                     "Disagree",
                                     "Neutral",
                                     "Agree",
                                     "Strongly agree"))
  )
})

test_that("labels for second column are in the correct order", {
  expect_identical(unique(dummy_output[[2]]),
                   factor(c(0,1,2,3,4,5,6),
                          levels = c(0,1,2,3,4,5,6))
  )
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[dummy_output["RAP_implementing"] == "Strongly disagree",]$n, c(1, 1, 0, 0, 0, 0, 0))
  expect_equal(dummy_output[dummy_output["RAP_implementing"] == "Disagree",]$n, c(0, 1, 2, 0, 0, 0, 0))
  expect_equal(dummy_output[dummy_output["RAP_implementing"] == "Neutral",]$n, c(0, 0, 1, 3, 0, 0, 0))
  expect_equal(dummy_output[dummy_output["RAP_implementing"] == "Agree",]$n, c(0, 0, 0, 1, 4, 0, 0))
  expect_equal(dummy_output[dummy_output["RAP_implementing"] == "Strongly agree",]$n, c(0, 0, 0, 0, 0, 2, 1))
})
