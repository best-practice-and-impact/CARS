dummy_data <- data.frame(prac_use_open_source = c(rep("Never", 3), rep("Sometimes", 2), rep(NA, 1)),
                         prac_open_source_own = c(rep("Sometimes", 3), rep("I don't understand this question", 2), rep("All the time", 1)),
                         prac_version_control = c(rep("Rarely", 3), rep("All the time", 2), rep("Never", 1)),
                         prac_review = c(rep("Regularly", 3), rep("All the time", 2), rep("Never", 1)),
                         prac_functions = c(rep("I don't understand this question", 3), rep("Never", 2), rep("Rarely", 1)),
                         prac_unit_test = c(rep("All the time", 3), rep("Rarely", 2), rep("Never", 1)),
                         prac_package = c(rep("Never", 3), rep("Sometimes", 2), rep("Rarely", 1)),
                         prac_dir_structure = c(rep("Sometimes", 3), rep("Rarely", 2), rep("Never", 1)),
                         prac_style = c(rep("Rarely", 3), rep("Never", 2), rep("Sometimes", 1)),
                         prac_automated_QA = c(rep("Regularly", 3), rep("Sometimes", 2), rep("Never", 1)),
                         prac_AQUA_book = c(rep("I don't understand this question", 3), rep("Never", 2), rep("Sometimes", 1)))

dummy_output <- summarise_coding_practices(dummy_data)

test_that("output is a dataframe", {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("output has sixty-six rows", {
  expect_equal(nrow(dummy_output), 66)
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
  expect_equal(unique(dummy_output[[1]]), c("I use open source software when programming",
                                            "My team open sources its code",
                                            "I use a source code version control system e.g. Git",
                                            "Code my team writes is reviewed by a colleague",
                                            "I write repetitive elements in my code as functions",
                                            "I unit test my code",
                                            "I collect my code and supporting material into packages",
                                            "I follow a standard directory structure when programming",
                                            "I follow coding guidelines or style guides when programming",
                                            "I write code to automatically quality assure data",
                                            "My team applies the principles set out in the Aqua book when carrying out analysis as code"))
})

test_that("frequencies are correct", {
  expect_true(all(subset(dummy_output, Response=="I don't understand this question", select=Count) == c(0, 2, 0, 0, 3, 0, 0, 0, 0, 0, 3)))
  expect_true(all(subset(dummy_output, Response=="Never", select=Count) == c(3, 0, 1, 1, 2, 1, 3, 1, 2, 1, 2)))
  expect_true(all(subset(dummy_output, Response=="Rarely", select=Count) == c(0, 0, 3, 0, 1, 2, 1, 2, 3, 0, 0)))
  expect_true(all(subset(dummy_output, Response=="Sometimes", select=Count) == c(2, 3, 0, 0, 0, 0, 2, 3, 1, 2, 1)))
  expect_true(all(subset(dummy_output, Response=="Regularly", select=Count) == c(0, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0)))
  expect_true(all(subset(dummy_output, Response=="All the time", select=Count) == c(0, 1, 2, 2, 0, 3, 0, 0, 0, 0, 0)))
})
