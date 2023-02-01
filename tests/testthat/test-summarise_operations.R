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

test_that("summarise_operations missing data is handled correctly", {

  got <- summarise_operations(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_operations output is as expected", {

  got <- summarise_operations(dummy_data)

  expected <- data.frame("name" = rep(c("Data analysis",
                                        "Data cleaning",
                                        "Data linking",
                                        "Data transfer / migration",
                                        "Data visualisation",
                                        "Machine learning",
                                        "Modelling",
                                        "Quality assurance"), each=2),
                         "value" = factor(rep(c("I do some or all of this by coding",
                                                "I do this without coding"), 8),
                                          levels = c("I do some or all of this by coding",
                                                     "I do this without coding")),
                         "n" = c(3/4, 1/4, 1/2, 1/2, 1/5, 4/5, 1, 0,
                                 3/5, 2/5, 1/2, 1/2, 3/5, 2/5, 1/4, 3/4))

  expect_equal(got, expected)

})
