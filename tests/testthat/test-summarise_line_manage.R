
test_that("summarise_line_manage", {

  dummy_data <- data.frame(management = c(NA,
                                          rep("Yes", 2),
                                          rep("No - I manage people who do not write code", 3),
                                          rep("No - I don't line manage anyone", 4)))

  got <- summarise_line_manage(dummy_data)

  expect_false(any(is.na.data.frame(got)))

  expected <- data.frame(name = rep("Line manage anyone who writes codes", 3),
                         value =  factor(c("Yes",
                                           "No - I manage people who do not write code",
                                           "No - I don't line manage anyone"),
                                         levels = c("Yes",
                                                    "No - I manage people who do not write code",
                                                    "No - I don't line manage anyone")),
                         n = c(0.22, 0.33, 0.44))

  expect_equal(got, expected)

})
