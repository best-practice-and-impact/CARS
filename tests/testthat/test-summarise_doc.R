dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 5)),
                         doc_comments = c("Sometimes", "Never", "Never", "Regularly", "All the time", "I don't understand this question"),
                         doc_functions = c("All the time", "I don't understand this question", "Never", "Rarely", "Sometimes", "Regularly"),
                         doc_readme = c("Regularly", "All the time", "I don't understand this question", "Never", "Rarely", "Sometimes"),
                         doc_desk_notes = c("I don't understand this question", "Never", "Rarely", "Sometimes", "Regularly", "All the time"),
                         doc_registers = c("Rarely", "Sometimes", "Regularly", "All the time", "I don't understand this question", "Never"),
                         doc_AQA_logs = c("Sometimes", "Regularly", "All the time", "I don't understand this question", "Never", "Rarely"),
                         doc_flow_charts = c("Never", NA, "Rarely", "Sometimes", "Regularly", "All the time")
)

dummy_output <- summarise_doc(dummy_data)

test_that("output is a dataframe", {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("output has fourty-two rows", {
  expect_equal(nrow(dummy_output), 42)
})

test_that("output does not contain missing values", {
  expect_false(any(is.na.data.frame(dummy_output)))
})

test_that("output has the correct column order", {
  expect_equal(colnames(dummy_output), c("name",
                                         "value",
                                         "n"))
})

test_that("output has the correct question names", {
  expect_equal(unique(dummy_output[[1]]), c("Analytical Quality Assurance (AQA) logs",
                                            "Code comments",
                                            "Data or assumptions registers",
                                            "Desk notes",
                                            "Documentation for each function or class",
                                            "Flow charts",
                                            "README files"))
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[dummy_output$value == "I don't understand this question",]$n, c(1, 1, 1, 0, 1, 0, 1))
  expect_equal(dummy_output[dummy_output$value == "Never",]$n, c(1, 2, 1, 1, 1, 0, 1))
  expect_equal(dummy_output[dummy_output$value == "Rarely",]$n, c(0, 0, 1, 1, 1, 1, 1))
  expect_equal(dummy_output[dummy_output$value == "Sometimes",]$n, c(1, 0, 0, 1, 1, 1, 1))
  expect_equal(dummy_output[dummy_output$value == "Regularly",]$n, c(1, 1, 1, 1, 1, 1, 0))
  expect_equal(dummy_output[dummy_output$value == "All the time",]$n, c(1, 1, 1, 1, 0, 1, 1))
})
