
test_that("Outcome is as expected", {

  dummy_data <- data.frame(use_open_source_score = c(1, 3, 5, 7),
                           open_code_score = c(2, 4, 6, 8),
                           version_control_score = c(2, 4, 6, 8),
                           peer_review_score = c(1, 3, 5, 7),
                           AQUA_book_score = c(2, 4, 6, 8),
                           doc_score = c(1, 3, 5, 7),
                           basic_rap_score = c(3, 6, 2, 4),
                           function_score = c(2, 4, 6, 8),
                           unit_test_score = c(1, 3, 5, 7),
                           function_doc_score = c(2, 4, 6, 8),
                           package_score = c(1, 3, 5, 7),
                           code_style_score = c(2, 4, 6, 8),
                           cont_integreation_score = c(1, 3, 5, 7),
                           dep_management_score = c(2, 4, 6, 8),
                           advanced_rap_score = c(3, 7, 5, 6))

  got <-summarise_rap_comp(dummy_data)

  expected <- data.frame(Component = factor(c("AQUA book guidance",
                                              "Documentation",
                                              "Peer review",
                                              "Team open source code",
                                              "Use open source software",
                                              "Version control",
                                              "Code packages",
                                              "Continuous integration",
                                              "Dependency management",
                                              "Follow code style guidelines",
                                              "Function documentation",
                                              "Functions",
                                              "Unit testing"),
                                            levels = c("AQUA book guidance",
                                                       "Documentation",
                                                       "Peer review",
                                                       "Team open source code",
                                                       "Use open source software",
                                                       "Version control",
                                                       "Code packages",
                                                       "Continuous integration",
                                                       "Dependency management",
                                                       "Follow code style guidelines",
                                                       "Function documentation",
                                                       "Functions",
                                                       "Unit testing")),

                         Type = c("Basic", "Basic", "Basic", "Basic", "Basic", "Basic",
                                  "Advanced", "Advanced", "Advanced", "Advanced", "Advanced", "Advanced", "Advanced"),

                         Count = c(20, 16, 16, 20, 16, 20, 16, 16, 20, 20, 20, 20, 16)
  )

  expect_equal(got, expected)

})
