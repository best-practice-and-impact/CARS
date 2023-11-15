dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 4)),
                         prac_use_open_source = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         prac_open_source_own = c(NA, "All the time", "Regularly", "Sometimes", "Rarely"),
                         prac_version_control = c(NA, "Never" ,"All the time", "Regularly", "Sometimes"),
                         prac_review = c(NA, "Rarely", "Never", "All the time", "Regularly"),
                         prac_development_QA = c(NA, "Sometimes", "Rarely", "Never", "All the time"),
                         doc_readme = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         doc_comments = c(NA, "All the time", "Regularly", "Sometimes", "Rarely"))

test_that("derive_basic_rap_scores validation works", {

  dummy_data <- data.frame()

  expect_error(derive_basic_rap_scores(dummy_data), "Unexpected input - missing column names: code_freq\nprac_use_open_source\nprac_open_source_own\nprac_version_control\nprac_review\nprac_development_QA\ndoc_comments\ndoc_readme")

})

test_that("derive_basic_rap_scores output is as expected", {

  got <- derive_basic_rap_scores(dummy_data)

  expected <- data.frame(code_freq = c("Never", rep("Sometimes", 4)),
                         prac_use_open_source = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         prac_open_source_own = c(NA, "All the time", "Regularly", "Sometimes", "Rarely"),
                         prac_version_control = c(NA, "Never" ,"All the time", "Regularly", "Sometimes"),
                         prac_review = c(NA, "Rarely", "Never", "All the time", "Regularly"),
                         prac_development_QA = c(NA, "Sometimes", "Rarely", "Never", "All the time"),
                         doc_readme = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         doc_comments = c(NA, "All the time", "Regularly", "Sometimes", "Rarely"),
                         use_open_source_score = c(NA, 1, 0, 0, 0),
                         open_code_score = c(NA, 1, 1, 0, 0),
                         version_control_score = c(NA, 0, 1, 1, 0),
                         peer_review_score = c(NA, 0, 0, 1, 1),
                         development_QA_score = c(NA, 0, 0, 0, 1),
                         doc_score = c(NA, 1, 0, 0, 0),
                         basic_rap_score = c(NA, 3, 2, 2, 2))

  expect_equal(got, expected)

})
