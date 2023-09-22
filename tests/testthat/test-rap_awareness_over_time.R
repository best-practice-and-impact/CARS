
dummy_data <- data.frame(year = rep(c(2020, 2021, 2022), each=2),
                         heard_of_RAP = rep(c("Yes", "No"), times=3))

test_that("summarise_rap_awareness_over_time missing data is handled correctly", {

  dummy_data[1,2] <- NA

  got <- summarise_rap_awareness_over_time(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_awareness_over_time output is as expected", {

  got <- summarise_rap_awareness_over_time(dummy_data)

  expected <- data.frame(Var1 = factor(c("Yes", "Yes", "Yes"),
                                       levels = c("No", "Yes")),
                         Var2 = factor(c("2020", "2021", "2022"),
                                       levels = c("2020", "2021", "2022")),
                         Freq = c(1, 1, 1),
                         n = c(2, 2, 2)) %>%
    get_ci(freq_col = 3, n_col = 4)

  expect_equal(got, expected)

})
