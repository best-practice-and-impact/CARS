
dummy_data <- data.frame(

  code_freq = rep(c(
    "Never",
    "Rarely",
    "Sometimes",
    "Regularly",
    "All the time"),
    each = 3),

  use_open_source_score = rep(c(NA, 1, 0), times = 5),
  open_code_score = rep(c(NA, 1, 0), times = 5),
  version_control_score = rep(c(NA, 1, 0), times = 5),
  peer_review_score = rep(c(NA, 1, 0), times = 5),
  development_QA_score = rep(c(NA, 1, 0), times = 5),
  doc_score = rep(c(NA, 1, 0), times = 5),
  basic_rap_score = rep(c(NA, 1, 0), times = 5),
  function_score = rep(c(NA, 1, 0), times = 5),
  unit_test_score = rep(c(NA, 1, 0), times = 5),
  function_doc_score = rep(c(NA, 1, 0), times = 5),
  package_score = rep(c(NA, 1, 0), times = 5),
  code_style_score = rep(c(NA, 1, 0), times = 5),
  cont_integration_score = rep(c(NA, 1, 0), times = 5),
  dep_management_score = rep(c(NA, 1, 0), times = 5),
  advanced_rap_score =rep(c(NA, 1, 0), times = 5)

)

test_that("summarise_rap_comp missing data is handled correctly", {

  dummy_data[7] <- NA

  got <-summarise_rap_comp(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_comp output is as expected", {

  got <-summarise_rap_comp(dummy_data)

  expected <- data.frame(

    name = factor(c(
      "Use open source software",
      "Team open source code",
      "Version control",
      "Peer review",
      "Development QA",
      "Documentation",
      "Functions",
      "Unit testing",
      "Function documentation",
      "Code packages",
      "Follow code style guidelines",
      "Continuous integration",
      "Dependency management"),
       levels = c(
         "Use open source software",
         "Team open source code",
         "Version control",
         "Peer review",
         "Development QA",
         "Documentation",
         "Functions",
         "Unit testing",
         "Function documentation",
         "Code packages",
         "Follow code style guidelines",
         "Continuous integration",
         "Dependency management")),

    value = c(
      rep("Basic", times = 6),
      rep("Advanced", times = 7)),

    n = rep(5/12, times = 13)

  )

  expect_equal(got, expected)

})
