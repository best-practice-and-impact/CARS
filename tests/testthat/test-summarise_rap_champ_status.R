
dummy_data <- data.frame(

  RAP_champ_status = c(
    NA,
    "Yes, and I am a RAP Champion",
    "Yes, and I know who the RAP Champion is",
    "Yes, but I don't know who the RAP Champion is",
    "No",
    "I don't know")

)

test_that("summarise_rap_champ_status missing data is handled correctly", {

  got <- summarise_rap_champ_status(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_knowledge output is as expected", {

  got <- summarise_rap_champ_status(dummy_data)

  expected <- data.frame(

    value = factor(c(
      "Yes, and I am a RAP Champion",
      "Yes, and I know who the RAP Champion is",
      "Yes, but I don't know who the RAP Champion is",
      "No",
      "I don't know"),
      levels = c(
        "Yes, and I am a RAP Champion",
        "Yes, and I know who the RAP Champion is",
        "Yes, but I don't know who the RAP Champion is",
        "No",
        "I don't know")),

    n = rep(1/5, times = 5)

  )

  expect_equal(got, expected)

})
