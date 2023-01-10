dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 4)),
                         prac_use_open_source = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"),
                         prac_open_source_own = c("Never", "All the time", "Regularly", "Sometimes", "Rarely"),
                         prac_version_control = c("Rarely", "Never" ,"All the time", "Regularly", "Sometimes"),
                         prac_review = c("Sometimes", "Rarely", "Never", "All the time", "Regularly"),
                         prac_AQUA_book = c("Regularly", "Sometimes", "Rarely", "Never", "All the time"),
                         doc_readme = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"),
                         doc_comments = c("Never", "All the time", "Regularly", "Sometimes", "Rarely"))

test_that("derive_basic_rap_scores validation works", {

  dummy_data <- data.frame()

  expect_error(derive_basic_rap_scores(dummy_data), "Unexpected input - missing column names: code_freq\nprac_use_open_source\nprac_open_source_own\nprac_version_control\nprac_review\nprac_AQUA_book\ndoc_comments\ndoc_readme")

})

test_that("derive_basic_rap_scores missing data is handled correctly", {
  dummy_data[2] <- NA

  got <- derive_basic_rap_scores(dummy_data)

  expect_false(any(is.na.data.frame(got[8:14])))
})

test_that("derive_basic_rap_scores output is as expected", {

  got <- derive_basic_rap_scores(dummy_data)

  expected <- data.frame(code_freq = rep("Sometimes", 4),
                         prac_use_open_source = c("Regularly", "Sometimes", "Rarely", "Never"),
                         prac_open_source_own = c("All the time", "Regularly", "Sometimes", "Rarely"),
                         prac_version_control = c("Never" ,"All the time", "Regularly", "Sometimes"),
                         prac_review = c("Rarely", "Never", "All the time", "Regularly"),
                         prac_AQUA_book = c("Sometimes", "Rarely", "Never", "All the time"),
                         doc_readme = c("Regularly", "Sometimes", "Rarely", "Never"),
                         doc_comments = c("All the time", "Regularly", "Sometimes", "Rarely"),
                         use_open_source_score = c(1, 0, 0, 0),
                         open_code_score = c(1, 1, 0, 0),
                         version_control_score = c(0, 1, 1, 0),
                         peer_review_score = c(0, 0, 1, 1),
                         AQUA_book_score = c(0, 0, 0, 1),
                         doc_score = c(1, 0, 0, 0),
                         basic_rap_score = c(3, 2, 2, 2))

  expect_equal(got, expected)

})
