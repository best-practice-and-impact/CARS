dummy_data <- data.frame(

  code_freq = rep(c(
    NA,
    "Never",
    "Rarely",
    "Sometimes",
    "Regularly",
    "All the time"),
    each=18),

  other_coding_experience = rep(c(
    NA,
    "Yes",
    "No"),
    times = 6,
    each = 6),

  first_learned = rep(c(
    NA,
    "Current employment",
    "Education",
    "Previous private sector employment",
    "Previous public sector employment",
    "Other"),
    times = 18)

)

test_that("summarise_where_learned_code missing data is handled correctly", {

  got <- summarise_where_learned_code(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_where_learned_code output is as expected", {

  got <- summarise_where_learned_code(dummy_data)

  expected <- data.frame(

    value = factor(c(
      "Current employment",
      "Education",
      "Previous private sector employment",
      "Previous public sector employment",
      "Other"),
      levels = c(
        "Current employment",
        "Education",
        "Previous private sector employment",
        "Previous public sector employment",
        "Other")),

    n = c(19/47, rep(7/47, times=4))

  )

  expect_equal(got, expected)

})


test_that("summarise_where_learned_code validation works", {

  dummy_data_1 <- data.frame(
    code_freq = c(rep("Never", 8), "Sometimes", "Regularly"),
    other_coding_experience = c(rep("Yes", 8), "No", "Yes"),
    prev_coding_experience = c(rep("Yes", 8), NA, "No")
  )

  expect_error(summarise_where_learned_code(dummy_data_1),
               "unexpected_input: no column called 'first_learned'")

  dummy_data_2 <- data.frame(
    code_freq = c(rep("Never", 8), "Sometimes", "Regularly"),
    prev_coding_experience = c(rep("Yes", 8), NA, "No"),
    first_learned = c(rep("Self-taught" , 3),
                      rep( "In public sector employment", 3),
                      rep("other" , 1),
                      rep(NA , 3))
  )

  expect_error(summarise_where_learned_code(dummy_data_2),
               "unexpected_input: no column called 'other_coding_experience'")

  dummy_data_3 <- data.frame(
    other_coding_experience = c(rep("Yes", 8), "No", "Yes"),
    prev_coding_experience = c(rep("Yes", 8), NA, "No"),
    first_learned = c(rep("Self-taught" , 3),
                      rep( "In public sector employment", 3),
                      rep("other" , 1),
                      rep(NA , 3))
  )

  expect_error(summarise_where_learned_code(dummy_data_3),
               "unexpected_input: no column called 'code_freq'")

})
