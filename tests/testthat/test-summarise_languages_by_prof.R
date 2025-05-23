# Coding tools frequency tables (access or knowledge)

knowledge_response <- rep(c(
  NA,
  "Yes",
  "No",
  "Not required for my work"),
  each = 3, times = 6)

prof_response <- rep(c(
  NA,
  "Yes",
  "No"),
  times = 24)

dummy_data <- data.frame(

  code_freq = rep(c(
    NA,
    "Never",
    "Rarely",
    "Sometimes",
    "Regularly",
    "Always"),
    each = 12),

  knowledge_R = knowledge_response,
  knowledge_SQL = knowledge_response,
  knowledge_SAS = knowledge_response,
  knowledge_VBA = knowledge_response,
  knowledge_python = knowledge_response,
  knowledge_SPSS = knowledge_response,
  knowledge_stata = knowledge_response,
  knowledge_matlab = knowledge_response,

  prof_DE = prof_response,
  prof_DS = prof_response,
  prof_GDD = prof_response,
  prof_GAD = prof_response,
  prof_GES = prof_response,
  prof_geog = prof_response,
  prof_GORS = prof_response,
  prof_GSE = prof_response,
  prof_GSR = prof_response,
  prof_GSG = prof_response

)

test_that("summarise_languages_by_prof missing data is handled correctly", {

  got <- summarise_languages_by_prof(dummy_data, config, question = "professions")


  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_languages_by_prof output is as expected", {

  got <- summarise_languages_by_prof(dummy_data, config, question = "professions")

  expected <- data.frame(

    Language = rep(c(
      "Python",
      "R",
      "Matlab",
      "SAS",
      "SPSS",
      "SQL",
      "Stata",
      "VBA"),
      times = 10),

    Profession = rep(c(
      "Data Engineers",
      "Data Scientists",
      "Government Digital and Data",
      "Government Actuary's Department",
      "Government Economic Service",
      "Government Geography Profession",
      "Government Operational Research Service",
      "Government Science & Engineering",
      "Government Social Research",
      "Government Statistician Group"),
      each = 8),

    n = rep(1/3, times = 80)

  )

  expect_equal(got, expected)

})
