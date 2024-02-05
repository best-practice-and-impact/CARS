# Coding tools frequency tables (access or knowledge)

prac_responses <- rep(c(
  NA,
  "I don't understand this question",
  "Never",
  "Rarely",
  "Sometimes",
  "Regularly",
  "All the time"),
  each = 3)

prof_responses <- rep(c(
  NA,
  "Yes",
  "No"),
  times = 7)

dummy_data <- data.frame(

  prac_use_open_source = prac_responses,
  prac_open_source_own = prac_responses,
  prac_version_control = prac_responses,
  prac_review = prac_responses,
  prac_functions = prac_responses,
  prac_unit_test = prac_responses,
  prac_package = prac_responses,
  prac_dir_structure = prac_responses,
  prac_style = prac_responses,
  prac_automated_QA = prac_responses,
  prac_development_QA = prac_responses,
  prac_proportionate_QA = prac_responses,

  prof_DE = prof_responses,
  prof_DS = prof_responses,
  prof_DDAT = prof_responses,
  prof_GAD = prof_responses,
  prof_GES = prof_responses,
  prof_geog = prof_responses,
  prof_GORS = prof_responses,
  prof_GSR = prof_responses,
  prof_GSG = prof_responses

)

test_that("summarise_open_source_by_prof missing data is handled correctly", {

  got <- summarise_open_source_by_prof(dummy_data)


  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_open_source_by_prof output is as expected", {

  got <- summarise_open_source_by_prof(dummy_data)

  expected <- data.frame(

    name = rep(c(
      "Data engineers",
      "Data scientists",
      "Digital and data (DDAT)",
      "Actuaries",
      "Economists (GES)",
      "Geographers",
      "Operational researchers (GORS)",
      "Social researchers (GSR)",
      "Statisticians (GSG)"),
      each=6),

    value = factor(rep(c(
      "I don't understand this question",
      "Never",
      "Rarely",
      "Sometimes",
      "Regularly",
      "All the time"), 9),
      levels = c(
        "I don't understand this question",
        "Never",
        "Rarely",
        "Sometimes",
        "Regularly",
        "All the time")),

    n = rep(1/6, times = 54)

  )

  expect_equal(got, expected)

})
