
dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 4)),
                         prac_functions = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         prac_manual_tests = c(NA, "Always", "Regularly", "Sometimes", "Rarely"),
                         prac_auto_tests = c(NA, "Always", "Regularly", "Sometimes", "Rarely"),
                         doc_function = c(NA, "Never", "Always", "Regularly", "Sometimes"),
                         prac_control_flow = c(NA, "Rarely", "Never", "Always", "Regularly"),
                         prac_config = c(NA, "Sometimes", "Rarely", "Never", "Always"),
                         prac_code_style = c(NA, "Sometimes", "Rarely", "Never", "Always"),
                         doc_dependencies = c(NA, "Rarely", "Always", "Never", "Regularly"))

test_that("derive_advanced_rap_scores validation works", {

  dummy_data <- data.frame()

  expect_error(derive_advanced_rap_scores(dummy_data), "Unexpected input - missing column names: code_freq\nprac_functions\nprac_manual_tests\nprac_auto_tests\ndoc_function\nprac_control_flow\nprac_config\nprac_code_style\ndoc_dependencies")

})

test_that("derive_advanced_rap_scores output is as expected", {

  got <- derive_advanced_rap_scores(dummy_data)

  expected <- data.frame(code_freq = c("Never", rep("Sometimes", 4)),
                         prac_functions = c(NA, "Regularly", "Sometimes", "Rarely", "Never"),
                         prac_manual_tests = c(NA, "Always", "Regularly", "Sometimes", "Rarely"),
                         prac_auto_tests = c(NA, "Always", "Regularly", "Sometimes", "Rarely"),
                         doc_function = c(NA, "Never", "Always", "Regularly", "Sometimes"),
                         prac_control_flow = c(NA, "Rarely", "Never", "Always", "Regularly"),
                         prac_config = c(NA, "Sometimes", "Rarely", "Never", "Always"),
                         prac_code_style = c(NA, "Sometimes", "Rarely", "Never", "Always"),
                         doc_dependencies = c(NA, "Rarely", "Always", "Never", "Regularly"),
                         function_score = c(NA, 1, 0, 0, 0),
                         manual_test_score = c(NA, 1, 1, 0, 0),
                         auto_test_score = c(NA, 1, 1, 0, 0),
                         function_doc_score = c(NA, 0, 1, 1, 0),
                         control_flow_score = c(NA, 0, 0, 1, 1),
                         config_score = c(NA, 0, 0, 0, 1),
                         code_style_score = c(NA, 0, 0, 0, 1),
                         dep_management_score = c(NA, 0, 1, 0, 1),
                         advanced_rap_score = c(NA, 3, 4, 2, 4))

  expect_equal(got, expected)

})
