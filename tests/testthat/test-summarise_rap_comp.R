
dummy_data <- data.frame(code_freq = c(rep("Somtimes", 4)),
                         use_open_source_score = c(6, NA, 5, 7),
                         open_code_score = c(2, 4, 6, 8),
                         version_control_score = c(2, 4, 6, 8),
                         peer_review_score = c(6, 5, 5, 7),
                         AQUA_book_score = c(2, 4, 2, 3),
                         doc_score = c(1, 3, 5, 7),
                         basic_rap_score = c(3, 6, 2, 4),
                         function_score = c(5, 7, 6, 8),
                         unit_test_score = c(1, 3, 5, 7),
                         function_doc_score = c(2, 4, 6, 8),
                         package_score = c(1, 3, 5, 7),
                         code_style_score = c(2, 4, 6, 8),
                         cont_integreation_score = c(1, 3, 5, 7),
                         dep_management_score = c(2, 4, 5, 3),
                         advanced_rap_score = c(3, 7, 5, 6))

test_that("summarise_rap_comp missing data is handled correctly", {

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

                         n = c(0.08, 0.08, 0.08, 0.10, 0.05, 0.07, 0.11,
                               0.07, 0.08, 0.07, 0.08, 0.07, 0.06)
  )

  expect_equal(got, expected)

})
