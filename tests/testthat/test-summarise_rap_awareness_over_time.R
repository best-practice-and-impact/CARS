heard_of_RAP <- c(NA,
                  rep(c("Yes",
                        "No"),
                      times = 6))

year <- c(NA,
          rep(c("2020",
                "2021",
                "2022"),
              each = 4))

dummy_data <- data.frame(heard_of_RAP, year)

test_that("summarise_rap_awareness_over_time missing data is handled correctly", {

  got <- summarise_rap_awareness_over_time(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_awareness_over_time output is as expected", {

  got <- summarise_rap_awareness_over_time(dummy_data)

  expected <- data.frame(Var1 = factor(rep("Yes", 3),
                                       levels = c("No", "Yes")),
                         Var2 = factor(c("2020", "2021", "2022"),
                                       levels = c("2020", "2021", "2022")),
                         Freq = c(2),
                         n = c(4),
                         percent = c(0.5),
                         lower = c(0.15003899),
                         upper = c(0.849961),
                         lower_ci = c(0.34996101),
                         upper_ci = c(0.34996101))

  expect_equal(got, expected)


})


