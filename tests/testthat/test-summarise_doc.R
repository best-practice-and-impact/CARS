
dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 5)),
                         doc_comments = c("Sometimes", "Never", "Never", "Regularly", "All the time", "I don't understand this question"),
                         doc_functions = c("All the time", "I don't understand this question", "Never", "Rarely", "Sometimes", "Regularly"),
                         doc_readme = c("Regularly", "All the time", "I don't understand this question", "Never", "Rarely", "Sometimes"),
                         doc_desk_notes = c("I don't understand this question", "Never", "Rarely", "Sometimes", "Regularly", "All the time"),
                         doc_registers = c("Rarely", "Sometimes", "Regularly", "All the time", "I don't understand this question", "Never"),
                         doc_AQA_logs = c("Sometimes", "Regularly", "All the time", "I don't understand this question", "Never", "Rarely"),
                         doc_flow_charts = c("Never", NA, "Rarely", "Sometimes", "Regularly", "All the time")
)

test_that("summarise_doc validation works", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_doc(dummy_data), "unexpected_input: no column called 'code_freq'")

})

test_that("summarise_doc missing data is handled correctly", {

  got <- summarise_doc(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_doc output is as expected", {

  got <- summarise_doc(dummy_data)

  expected <- data.frame(name = c(rep("Analytical Quality Assurance (AQA) logs", 6),
                                  rep("Code comments", 6),
                                  rep("Data or assumptions registers", 6),
                                  rep("Desk notes", 6),
                                  rep("Documentation for each function or class", 6),
                                  rep("Flow charts", 6),
                                  rep("README files", 6)),
                         value = factor(c("I don't understand this question",
                                          "Never",
                                          "Rarely",
                                          "Sometimes",
                                          "Regularly",
                                          "All the time"),
                                        levels = c("I don't understand this question",
                                                   "Never",
                                                   "Rarely",
                                                   "Sometimes",
                                                   "Regularly",
                                                   "All the time")),
                         n = c(0.20, 0.20, 0.00, 0.20, 0.20, 0.20, 0.20,
                               0.40, 0.00, 0.00, 0.20, 0.20, 0.20, 0.20,
                               0.20, 0.00, 0.20, 0.20, 0.00, 0.20, 0.20,
                               0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20,
                               0.20, 0.00, 0.00, 0.00, 0.25, 0.25, 0.25,
                               0.25, 0.20, 0.20, 0.20, 0.20, 0.00, 0.20))

  expect_equal(got, expected)

})
