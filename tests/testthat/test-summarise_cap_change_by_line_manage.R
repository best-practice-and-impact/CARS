
dummy_data <- data.frame(

  management = rep(c(
    NA,
    "Yes",
    "No - I manage people who do not write code",
    "No - I don't line manage anyone"),
    each = 6),

  coding_ability_change = rep(c(
    NA,
    "It has become significantly worse",
    "It has become slightly worse",
    "It has stayed the same",
    "It has become slightly better",
    "It has become significantly better"),
  times=4)

)

test_that("summarise_cap_change_by_line_manage missing data is handled correctly", {

  got <- summarise_cap_change_by_line_manage(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_cap_change_by_line_manage output is as expected", {

  got <- summarise_cap_change_by_line_manage(dummy_data)

  expected <- data.frame(

    management = factor(rep(c(
      "Yes",
      "No - I manage people who do not write code",
      "No - I don't line manage anyone"),
      each=5),
      levels = c(
        "Yes",
        "No - I manage people who do not write code",
        "No - I don't line manage anyone")),

    coding_ability_change = factor(rep(c(
      "It has become significantly worse",
      "It has become slightly worse",
      "It has stayed the same",
      "It has become slightly better",
      "It has become significantly better"),
      times = 3),
      levels = c(
        "It has become significantly worse",
        "It has become slightly worse",
        "It has stayed the same",
        "It has become slightly better",
        "It has become significantly better")),

      n = rep(0.2, times = 15)

  )

  expect_equal(got, expected)

})
