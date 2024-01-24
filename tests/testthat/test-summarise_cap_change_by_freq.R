
dummy_data <- data.frame(
  coding_ability_change = rep(c(
    NA,
    "It has become significantly worse",
    "It has become slightly worse",
    "It has stayed the same",
    "It has become slightly better",
    "It has become significantly better"),
    each = 90),
  code_freq = rep(c(
    NA,
    "Sometimes",
    "All the time",
    "Rarely",
    "Regularly"),
    times = 108),
  other_coding_experience = rep(c(
    NA,
    "Yes",
    "No"),
    times = 180),
  first_learned = rep(c(NA,
                        "Current employment",
                        "Education",
                        "Previous public sector employment",
                        "Previous private sector employment",
                        "Other"),
                      times =90)
  )

test_that("summarise_cap_change_by_freq missing data is handled correctly", {

  got <- summarise_cap_change_by_freq(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_cap_change_by_freq output is as expected", {

  got <- summarise_cap_change_by_freq(dummy_data)

  expected <- data.frame(

    code_freq = factor(rep(c(
      "Rarely",
      "Sometimes",
      "Regularly",
      "All the time"),
      each = 5),
      levels = c(
        "Rarely",
        "Sometimes",
        "Regularly",
        "All the time")),

    coding_ability_change = factor(rep(c(
      "It has become significantly worse",
      "It has become slightly worse",
      "It has stayed the same",
      "It has become slightly better",
      "It has become significantly better"),
      times = 4),
      levels = c(
        "It has become significantly worse",
        "It has become slightly worse",
        "It has stayed the same",
        "It has become slightly better",
        "It has become significantly better")),

    n = rep(1/5, times = 20)

  )

  expect_equal(got, expected)

})
