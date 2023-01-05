dummy_data <- data.frame(prac_functions = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"),
                         prac_unit_test = c("Never", "All the time", "Regularly", "Sometimes", "Rarely"),
                         doc_functions = c("Rarely", "Never", "All the time", "Regularly", "Sometimes"),
                         prac_package = c("Sometimes", "Rarely", "Never", "All the time", "Regularly"),
                         prac_style = c("Regularly", "Sometimes", "Rarely", "Never", "All the time"),
                         CI = c("Yes", "No", "Yes", "No", "I don't know what dependency management is"),
                         dep_management = c("I don't know what continuous integration is", "Yes", "No", "Yes", "No"))

test_that("derive_advanced_rap_scores validation works", {

  dummy_data <- data.frame()

  expect_error(derive_advanced_rap_scores(dummy_data), "Unexpected input - missing column names: prac_functions\nprac_unit_test\ndoc_functions\nprac_package\nprac_style\nCI\ndep_management")

})

test_that("derive_advanced_rap_scores missing data is handled correctly", {
  dummy_data[1] <- NA

  got <- derive_advanced_rap_scores(dummy_data)

  expect_false(any(is.na.data.frame(got[8:15])))
})

test_that("derive_advanced_rap_scores output is as expected", {

  got <- derive_advanced_rap_scores(dummy_data)

  expected <- data.frame(prac_functions = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"),
                         prac_unit_test = c("Never", "All the time", "Regularly", "Sometimes", "Rarely"),
                         doc_functions = c("Rarely", "Never", "All the time", "Regularly", "Sometimes"),
                         prac_package = c("Sometimes", "Rarely", "Never", "All the time", "Regularly"),
                         prac_style = c("Regularly", "Sometimes", "Rarely", "Never", "All the time"),
                         CI = c("Yes", "No", "Yes", "No", "I don't know what dependency management is"),
                         dep_management = c("I don't know what continuous integration is", "Yes", "No", "Yes", "No"),
                         function_score = c(1, 1, 0, 0, 0),
                         unit_test_score = c(0, 1, 1, 0, 0),
                         function_doc_score = c(0, 0, 1, 1, 0),
                         package_score = c(0, 0, 0, 1, 1),
                         code_style_score = c(1, 0, 0, 0, 1),
                         cont_integreation_score = c(1, 0, 1, 0, 0),
                         dep_management_score = c(0, 1, 0, 1, 0),
                         advanced_rap_score = c(3, 3, 3, 3, 2))

  expect_equal(got, expected)

})
