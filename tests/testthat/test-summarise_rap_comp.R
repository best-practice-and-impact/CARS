
dummy_data <- data.frame(code_freq = c(rep("Somtimes", 4)),
                         use_open_source_score = c(1, NA, 1, 0),
                         open_code_score = c(1, 1, 1, 1),
                         version_control_score = c(0, 0, 0, 0),
                         peer_review_score = c(1, 1, 1, 0),
                         AQUA_book_score = c(0, 1, 0, 1),
                         doc_score = c(1, 1, 1, 0),
                         basic_rap_score = c(1, 1, 0, 1),
                         function_score = c(1, 0, 1, 0),
                         unit_test_score = c(1, 1, 1, 0),
                         function_doc_score = c(0, 1, 1, 0),
                         package_score = c(1, 1, 1, 0),
                         code_style_score = c(0, 1, 1, 0),
                         cont_integration_score = c(1, 1, 1, 0),
                         dep_management_score = c(0, 1, 1, 1),
                         advanced_rap_score = c(1, 0, 1, 1))

test_that("summarise_rap_comp missing data is handled correctly", {

  dummy_data[7] <- NA

  got <-summarise_rap_comp(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_rap_comp output is as expected", {

  got <-summarise_rap_comp(dummy_data)

  expected <- data.frame(name = factor(c("Use open source software",
                                         "Team open source code",
                                         "Version control",
                                         "Peer review",
                                         "AQUA book guidance",
                                         "Documentation",
                                         "Functions",
                                         "Unit testing",
                                         "Function documentation",
                                         "Code packages",
                                         "Follow code style guidelines",
                                         "Continuous integration",
                                         "Dependency management"),
                                       levels = c("Use open source software",
                                                  "Team open source code",
                                                  "Version control",
                                                  "Peer review",
                                                  "AQUA book guidance",
                                                  "Documentation",
                                                  "Functions",
                                                  "Unit testing",
                                                  "Function documentation",
                                                  "Code packages",
                                                  "Follow code style guidelines",
                                                  "Continuous integration",
                                                  "Dependency management")),

                         value = c("Basic", "Basic", "Basic", "Basic", "Basic", "Basic",
                                   "Advanced", "Advanced", "Advanced", "Advanced", "Advanced", "Advanced", "Advanced"),

                         n = c(1/2, 1, 0, 3/4, 1/2, 3/4, 1/2,
                               3/4, 1/2, 3/4, 1/2, 3/4, 3/4)
  )

  expect_equal(got, expected)

})
