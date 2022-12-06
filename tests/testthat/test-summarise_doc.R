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
  expect_equal(colnames(dummy_output), c("Question",
                                         "Response",
                                         "Count"))
})

test_that("output has the correct question names", {
  expect_equal(unique(dummy_output[[1]]), c("Code comments",
                                            "Documentation for each function or class",
                                            "README files",
                                            "Desk notes",
                                            "Analytical Quality Assurance (AQA) logs",
                                            "Data or assumptions registers",
                                            "Flow charts"))
})

test_that("frequencies are correct", {
  expect_true(all(subset(dummy_output, Response=="I don't understand this question", select=Count) == c(1, 1, 1, 0, 1, 1, 0)))
  expect_true(all(subset(dummy_output, Response=="Never", select=Count) == c(2, 1, 1, 1, 1, 1, 0)))
  expect_true(all(subset(dummy_output, Response=="Rarely", select=Count) == c(0, 1, 1, 1, 0, 1, 1)))
  expect_true(all(subset(dummy_output, Response=="Sometimes", select=Count) == c(0, 1, 1, 1, 1, 0, 1)))
  expect_true(all(subset(dummy_output, Response=="Regularly", select=Count) == c(1, 1, 0, 1, 1, 1, 1)))
  expect_true(all(subset(dummy_output, Response=="All the time", select=Count) == c(1, 0, 1, 1, 1, 1, 1)))
})
