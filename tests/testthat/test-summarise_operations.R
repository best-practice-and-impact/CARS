# Data operations table
# Frequency table should not include missing values and should include counts of 0

dummy_data <- data.frame(ops_analysis = c(rep("I do some or all of this by coding", 3), rep("I don't do this", 2), "I do this without coding", NA),
                         ops_cleaning = c(rep("I do some or all of this by coding", 2), rep("I don't do this", 3), rep("I do this without coding", 2)),
                         ops_linking = c(rep("I do some or all of this by coding", 1), rep("I don't do this", 2), rep("I do this without coding", 4)),
                         ops_transfer_migration = c(rep("I do some or all of this by coding", 4), rep("I don't do this", 3)),
                         ops_vis = c(rep("I do some or all of this by coding", 3), rep("I don't do this", 2), rep("I do this without coding", 2)),
                         ops_machine_learning = c(rep("I do some or all of this by coding", 2), rep("I don't do this", 3), rep("I do this without coding", 2)),
                         ops_modelling = c(rep("I do some or all of this by coding", 3), rep("I don't do this", 2), rep("I do this without coding", 2)),
                         ops_QA = c(rep("I do some or all of this by coding", 1), rep("I don't do this", 3), rep("I do this without coding", 3))
)

dummy_output <- summarise_operations(dummy_data)

test_that("output is a dataframe", {
  expect_s3_class(dummy_output, "data.frame")
})

test_that("output has three columns", {
  expect_equal(ncol(dummy_output), 3)
})

test_that("output does not contain missing values", {
  expect_false(any(is.na.data.frame(dummy_output)))
})

test_that("output has the correct column order", {
  expect_equal(colnames(dummy_output), c("name", "value", "n"))
})

test_that("output has the correct question names", {
  expect_equal(dummy_output[[1]], c(rep("Data analysis",3), rep("Data cleaning",3), rep("Data linking",3), rep("Data transfer / migration",3), rep("Data visualisation", 3), rep("Machine learning",3), rep("Modelling",3), rep("Quality assurance",3)))
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[[3]], c(3,1,2,2,2,3,1,4,2,4,0,3,3,2,2,2,2,3,3,2,2,1,3,3))
})
