
dummy_data <- data.frame(
  ability_change = rep(c(
    NA,
    "It has become significantly worse",
    "It has become slightly worse",
    "It has stayed the same",
    "It has become slightly better",
    "It has become significantly better"),
    each = 60),
  code_freq = rep(c(
    NA,
    "Sometimes",
    "Always",
    "Rarely",
    "Regularly"),
    times = 72),
  coding_exp = rep(c(
    NA,
    "Yes",
    "No"),
    times = 120),
  first_learned = rep(c(NA,
                        "Current role",
                        "Education",
                        "Previous public sector employment"),
                      times = 90)
  )

test_that("summarise_cap_change_by_freq missing data is handled correctly", {

  got <- summarise_cap_change_by_freq(dummy_data, config, question1 = "code_freq", question2 = "ability_change")

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_cap_change_by_freq output is as expected", {

  got <- summarise_cap_change_by_freq(dummy_data, config, question1 = "code_freq", question2 = "ability_change")

  expected <- data.frame(

    code_freq = factor(rep(c(
      "Rarely",
      "Sometimes",
      "Regularly",
      "Always"),
      each = 5),
      levels = c(
        "Rarely",
        "Sometimes",
        "Regularly",
        "Always")),

    ability_change = factor(rep(c(
      "It has become significantly better",
      "It has become slightly better",
      "It has stayed the same",
      "It has become slightly worse",
      "It has become significantly worse"),
      times = 4),
      levels = c(
        "It has become significantly better",
        "It has become slightly better",
        "It has stayed the same",
        "It has become slightly worse",
        "It has become significantly worse")),

    n = rep(1/20, times = 20)

  )

  expect_equal(got, expected)

})
