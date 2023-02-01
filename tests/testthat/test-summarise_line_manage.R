
dummy_data <- data.frame(management = c(NA,
                                        rep("Yes", 2),
                                        rep("No - I manage people who do not write code", 3),
                                        rep("No - I don't line manage anyone", 4)))

test_that("summarise_line_manage missing data is handled correctly", {

  got <- summarise_line_manage(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_line_manage output is as expected", {

  got <- summarise_line_manage(dummy_data)

  expected <- data.frame(value =  factor(c("Yes",
                                           "No - I manage people who do not write code",
                                           "No - I don't line manage anyone"),
                                         levels = c("Yes",
                                                    "No - I manage people who do not write code",
                                                    "No - I don't line manage anyone")),
                         n = c(2/9, 1/3, 4/9))

  expect_equal(got, expected)

})
