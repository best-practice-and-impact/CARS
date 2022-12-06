
dummy_data <- data.frame(heard_of_RAP = c("No", "Yes", "Yes", "Yes", "Yes"),
                         RAP_confident = c("Strongly Agree" , "Agree", "Neutral", "Disagree", "Strongly Disagree"),
                         RAP_supported = c("Strongly Disagree", "Strongly Agree" , "Agree", "Neutral", "Disagree"),
                         RAP_resources = c("Disagree", "Strongly Disagree", "Strongly Agree" , "Agree", "Neutral"),
                         RAP_understand_key_components = c("Neutral", "Disagree", "Disagree", "Strongly Agree" , "Agree"),
                         RAP_important = c("Strongly Agree" , "Agree", NA, "Disagree", "Strongly Agree" ),
                         RAP_implementing = c("Strongly Agree" , "Agree", "Neutral", "Disagree", "Strongly Disagree"),
                         RAP_planning_to_implement = c("Strongly Disagree", "Strongly Agree" , "Agree", "Neutral", "Disagree"))


dummy_output <- summarise_rap_opinions(dummy_data)

test_that("Check output is dataframe" , {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output does not contain missing values", {
  expect_false(any(is.na(dummy_output)))
})

test_that("Check number of rows in output", {
  expect_equal(nrow(dummy_output), 35)
})

test_that("Check number of columns in output", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("Output column names are correct", {
  expect_equal(colnames(dummy_output), c("Question",
                                         "Response",
                                         "Count"))
})

test_that("Labels are in correct order",{
  expect_identical(unique(dummy_output[[1]]),
                   c("I feel confident implementing RAP in my work",
                     "I feel supported to implement RAP in my work",
                     "I know where to find resources to help me implement RAP",
                     "I understand what the key components of the RAP methodology are",
                     "I think it is important to implement RAP in my work",
                     "I and/or my team are currently implementing RAP",
                     "I or my team are planning on implementing RAP in the next 12 months"))
})


test_that("Check output values are correct",{
  expect_true(all(subset(dummy_output, Response=="Strongly disagree", select=Count) == c(1, 0, 1, 0, 0, 1, 0)))
  expect_true(all(subset(dummy_output, Response=="Disagree", select=Count) == c(1, 1, 0, 2, 1, 1, 1)))
  expect_true(all(subset(dummy_output, Response=="Neutral", select=Count) == c(1, 1, 1, 0, 0, 1, 1)))
  expect_true(all(subset(dummy_output, Response=="Agree", select=Count) == c(1, 1, 1, 1, 1, 1, 1)))
  expect_true(all(subset(dummy_output, Response=="Strongly agree", select=Count) == c(0, 1, 1, 1, 1, 0, 1)))

})
