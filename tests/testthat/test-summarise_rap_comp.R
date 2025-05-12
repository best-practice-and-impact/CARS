
dummy_data <- data.frame(

  code_freq = rep(c(
    "Never",
    "Rarely",
    "Sometimes",
    "Regularly",
    "Always"),
    each = 3),

  use_open_source_score = rep(c(NA, 1, 0), times = 5),
  open_code_score = rep(c(NA, 1, 0), times = 5),
  version_control_score = rep(c(NA, 1, 0), times = 5),
  peer_review_score = rep(c(NA, 1, 0), times = 5),
  qa_score = rep(c(NA, 1, 0), times = 5),
  doc_score = rep(c(NA, 1, 0), times = 5),
  basic_rap_score = rep(c(NA, 1, 0), times = 5),
  function_score = rep(c(NA, 1, 0), times = 5),
  manual_test_score = rep(c(NA, 1, 0), times = 5),
  auto_test_score = rep(c(NA, 1, 0), times = 5),
  function_doc_score = rep(c(NA, 1, 0), times = 5),
  control_flow_score = rep(c(NA, 1, 0), times = 5),
  config_score = rep(c(NA, 1, 0), times = 5),
  code_style_score = rep(c(NA, 1, 0), times = 5),
  dep_management_score = rep(c(NA, 1, 0), times = 5),
  advanced_rap_score =rep(c(NA, 1, 0), times = 5)

)


test_that("summarise_rap_comp missing data is handled correctly", {

  dummy_data[7] <- NA

  got <-summarise_rap_comp(dummy_data, config, question = "rap_components")

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_comp output is as expected", {

  got <- summarise_rap_comp(dummy_data, config, question = "rap_components", sample = FALSE)

  expected <- data.frame(

    name = factor(c(
      "Use open source software",
      "Open source own code",
      "Version control",
      "Peer review",
      "Work to quality standards",
      "Create project README",
      "Write functions",
      "Manually test code",
      "Write automated tests",
      "Document functions",
      "Use control flow",
      "Use configuration files",
      "Follow code style guidelines",
      "Document dependencies"),
       levels = c(
         "Use open source software",
         "Open source own code",
         "Version control",
         "Peer review",
         "Work to quality standards",
         "Create project README",
         "Write functions",
         "Manually test code",
         "Write automated tests",
         "Document functions",
         "Use control flow",
         "Use configuration files",
         "Follow code style guidelines",
         "Document dependencies")),

    value = c(
      rep("Basic", times = 6),
      rep("Advanced", times = 8)),

    n = rep(5/12, times = 14)

  )

  expect_equal(got, expected)

})
