dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 4)),
                         prac_open_source = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         work_publish_code = c(NA, "Always", "Regularly", "Sometimes", "Rarely"),
                         work_git = c(NA, "Never" ,"Always", "Regularly", "Sometimes"),
                         prac_review = c(NA, "Rarely", "Never", "Always", "Regularly"),
                         work_qa = c(NA, "Sometimes", "Rarely", "Never", "Always"),
                         doc_readme = c(NA, "Regularly", "Sometimes", "Rarely", "Never"))

test_that("derive_basic_rap_scores validation works", {

  dummy_data <- data.frame()

  expect_error(derive_basic_rap_scores(dummy_data), "Unexpected input - missing column names: code_freq\nprac_open_source\nwork_publish_code\nwork_git\nprac_review\nwork_qa\ndoc_readme")

})

test_that("derive_basic_rap_scores output is as expected", {

  got <- derive_basic_rap_scores(dummy_data)

  expected <- data.frame(code_freq = c("Never", rep("Sometimes", 4)),
                         prac_open_source = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         work_publish_code = c(NA, "Always", "Regularly", "Sometimes", "Rarely"),
                         work_git = c(NA, "Never" ,"Always", "Regularly", "Sometimes"),
                         prac_review = c(NA, "Rarely", "Never", "Always", "Regularly"),
                         work_qa = c(NA, "Sometimes", "Rarely", "Never", "Always"),
                         doc_readme = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         use_open_source_score = c(NA, 1, 0, 0, 0),
                         open_code_score = c(NA, 1, 1, 0, 0),
                         version_control_score = c(NA, 0, 1, 1, 0),
                         peer_review_score = c(NA, 0, 0, 1, 1),
                         qa_score = c(NA, 0, 0, 0, 1),
                         doc_score = c(NA, 1, 0, 0, 0),
                         basic_rap_score = c(NA, 3, 2, 2, 2))

  expect_equal(got, expected)

})
