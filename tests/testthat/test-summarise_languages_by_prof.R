# Coding tools frequency tables (access or knowledge)

dummy_data <- data.frame(
  code_freq = c(rep("Sometimes", 2), rep("All the time", 2), "Never", NA),
  knowledge_R = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  knowledge_SQL = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  knowledge_SAS = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  knowledge_VBA = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  knowledge_python = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  knowledge_SPSS = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  knowledge_stata = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  knowledge_JS = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  knowledge_java = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  knowledge_C = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  knowledge_matlab = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  prof_DS = c(rep("Yes", 3), rep("No", 3)),
  prof_DDAT = c("No", rep("Yes", 4), rep("No", 1)),
  prof_GAD = c(rep("No", 2), rep("Yes", 4)),
  prof_GES = c(rep("No", 2), rep("Yes", 2), rep("No", 2)),
  prof_geog = c(rep("No", 3), rep("Yes", 3)),
  prof_GORS = c(rep("No", 1), rep("Yes", 5)),
  prof_GSR = c(rep("Yes", 1), rep("No", 5)),
  prof_GSG = c(rep("Yes", 6)))

dummy_output <- summarise_languages_by_prof(dummy_data)

test_that("output is a dataframe", {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("output has eighty-eight rows", {
  expect_equal(nrow(dummy_output), 88)
})

test_that("output does not contain missing values", {
  expect_false(any(is.na.data.frame(dummy_output)))
})

test_that("output has the correct column order", {
  expect_equal(colnames(dummy_output), c("lang",
                                         "prof",
                                         "n"))
})
