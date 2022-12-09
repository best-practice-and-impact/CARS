# Coding tools frequency tables (access or knowledge)

dummy_data <- data.frame(
  knowledge_R = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_R = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_SQL = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_SQL = c("Yes", rep("No", 3), rep("Don't Know", 2)),
  knowledge_SAS = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  access_SAS = c(rep("Yes", 3), "No", rep("Don't Know", 2)),
  knowledge_VBA = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_VBA = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_python = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_python = c("Yes", rep("No", 3), rep("Don't Know", 2)),
  knowledge_SPSS = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  access_SPSS = c(rep("Yes", 3), "No", rep("Don't Know", 2)),
  knowledge_stata = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_stata = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_JS = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_JS = c("Yes", rep("No", 3), rep("Don't Know", 2)),
  knowledge_java = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  access_java = c(rep("Yes", 3), "No", rep("Don't Know", 2)),
  knowledge_C = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_C = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_matlab = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_matlab = c("Yes", rep("No", 5), rep("Don't Know", 0)) # Used to check zero counts aren't missing
)

dummy_data_missing_values <- dummy_data
dummy_data_missing_values[1,] <- NA

dummy_knowledge_output <- summarise_coding_tools(dummy_data, "knowledge")
dummy_access_output <- summarise_coding_tools(dummy_data, "access")
dummy_output_missing_values <- summarise_coding_tools(dummy_data_missing_values, "knowledge")

languages <- c(
  "C++ / C#",
  "Java / Scala",
  "Javascript / Typescript",
  "Matlab",
  "Python",
  "R",
  "SAS",
  "SPSS",
  "SQL",
  "Stata",
  "VBA"
)

test_that("output is a dataframe", {
  expect_s3_class(dummy_knowledge_output, "data.frame")
  expect_s3_class(dummy_access_output, "data.frame")
})

test_that("output has three columns", {
  expect_equal(ncol(dummy_knowledge_output), 3)
  expect_equal(ncol(dummy_access_output), 3)
})

test_that("output contains no missing values", {
  expect_false(any(is.na(dummy_output_missing_values)))
})

test_that("output has the correct column names", {
  expect_equal(colnames(dummy_access_output), c("name", "value", "n"))
  expect_equal(colnames(dummy_knowledge_output), c("name", "value", "n"))
})

test_that("programming language names are correct", {
  expect_equal(unique(dummy_access_output[[1]]), languages)
  expect_equal(unique(dummy_knowledge_output[[1]]), languages)
})

test_that("Frequencies are correct", {
  dummy_data <- dummy_data[order(tolower(colnames(dummy_data)))] # Sort alphabetically
  access_data <- dummy_data[1:11]
  knowledge_data <- dummy_data[12:22]

  expect_equal(dummy_knowledge_output[dummy_knowledge_output$value == "Yes",]$n, c(1,2,3,3,3,1,2,2,3,1,1))
  expect_equal(dummy_knowledge_output[dummy_knowledge_output$value == "Don't Know",]$n, c(3,1,1,1,1,3,1,1,1,3,3))
  expect_equal(dummy_knowledge_output[dummy_knowledge_output$value == "No",]$n, c(2,3,2,2,2,2,3,3,2,2,2))

  expect_equal(dummy_access_output[dummy_access_output$value == "Yes",]$n, c(2,3,1,1,1,2,3,3,1,2,2))
  expect_equal(dummy_access_output[dummy_access_output$value == "Don't Know",]$n, c(3,2,2,0,2,3,2,2,2,3,3))
  expect_equal(dummy_access_output[dummy_access_output$value == "No",]$n, c(1,1,3,5,3,1,1,1,3,1,1))
})
