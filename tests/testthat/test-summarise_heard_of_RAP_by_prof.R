# Coding tools frequency tables (access or knowledge)

dummy_data <- data.frame(
  workplace = rep(c(
    "Civil service, including devolved administrations",
    "Office for Students", "NHS"),
    each=4),
  heard_of_RAP = rep(c("Yes", "Yes", "No", "No"), 3),
  prof_DE = rep(c("Yes", "No"), 6),
  prof_DS = rep(c("Yes", "No"), 6),
  prof_DDAT = rep(c("Yes", "No"), 6),
  prof_GAD = rep(c("Yes", "No"), 6),
  prof_GES = rep(c("Yes", "No"), 6),
  prof_geog = rep(c("Yes", "No"), 6),
  prof_GORS = rep(c("Yes", "No"), 6),
  prof_GSR = rep(c("Yes", "No"), 6),
  prof_GSG = rep(c("Yes", "No"), 6)

)

test_that("summarise_heard_of_RAP_by_prof missing data is handled correctly", {

  dummy_data[1, ] <- NA

  got <- summarise_heard_of_RAP_by_prof(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_heard_of_RAP_by_prof output is as expected", {

  got <- summarise_heard_of_RAP_by_prof(dummy_data)

  expected <- data.frame(

    value = factor(c(
      "Data engineers",
      "Data scientists",
      "Digital and data (DDAT)",
      "Actuaries",
      "Economists (GES)",
      "Geographers",
      "Operational researchers (GORS)",
      "Social researchers (GSR)",
      "Statisticians (GSG)"),
      levels = c(
        "Data engineers",
        "Data scientists",
        "Digital and data (DDAT)",
        "Actuaries",
        "Economists (GES)",
        "Geographers",
        "Operational researchers (GORS)",
        "Social researchers (GSR)",
        "Statisticians (GSG)")),

    n = rep(1/2, times = 9)

  )

  expect_equal(got, expected)

})
