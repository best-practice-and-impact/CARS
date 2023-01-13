
dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 4)),
                         prac_functions = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         prac_unit_test = c(NA, "All the time", "Regularly", "Sometimes", "Rarely"),
                         doc_functions = c(NA, "Never", "All the time", "Regularly", "Sometimes"),
                         prac_package = c(NA, "Rarely", "Never", "All the time", "Regularly"),
                         prac_style = c(NA, "Sometimes", "Rarely", "Never", "All the time"),
                         CI = c(NA, "No", "Yes", "No", "I don't know what continuous integration is"),
                         dep_management = c(NA, "I don't know what dependency management is", "No", "Yes", "No"))

test_that("derive_advanced_rap_scores validation works", {

  dummy_data <- data.frame()

  expect_error(derive_advanced_rap_scores(dummy_data), "Unexpected input - missing column names: code_freq\nprac_functions\nprac_unit_test\ndoc_functions\nprac_package\nprac_style\nCI\ndep_management")

})

test_that("derive_advanced_rap_scores output is as expected", {

  got <- derive_advanced_rap_scores(dummy_data)

  expected <- data.frame(code_freq = c("Never", rep("Sometimes", 4)),
                         prac_functions = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         prac_unit_test = c(NA, "All the time", "Regularly", "Sometimes", "Rarely"),
                         doc_functions = c(NA, "Never", "All the time", "Regularly", "Sometimes"),
                         prac_package = c(NA, "Rarely", "Never", "All the time", "Regularly"),
                         prac_style = c(NA, "Sometimes", "Rarely", "Never", "All the time"),
                         CI = c(NA, "No", "Yes", "No", "I don't know what continuous integration is"),
                         dep_management = c(NA, "I don't know what dependency management is", "No", "Yes", "No"),
                         function_score = c(NA, 1, 0, 0, 0),
                         unit_test_score = c(NA, 1, 1, 0, 0),
                         function_doc_score = c(NA, 0, 1, 1, 0),
                         package_score = c(NA, 0, 0, 1, 1),
                         code_style_score = c(NA, 0, 0, 0, 1),
                         cont_integration_score = c(NA, 0, 1, 0, 0),
                         dep_management_score = c(NA, 0, 0, 1, 0),
                         advanced_rap_score = c(NA, 2, 3, 3, 2))

  expect_equal(got, expected)

})
