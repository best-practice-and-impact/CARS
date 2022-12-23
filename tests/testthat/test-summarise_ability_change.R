#
# test_that("Output is as expected", {
#
#   dummy_data <- data.frame(coding_ability_change = c(NA,
#                                                      rep("Significantly worse", 2),
#                                                      rep("Slightly worse", 3),
#                                                      rep("No change", 4),
#                                                      rep("Slightly better", 5),
#                                                      rep("Significantly better", 6)))
#
#   got <- summarise_ability_change(dummy_data)
#
#   expected <- data.frame(value = factor(c("Significantly worse",
#                                           "Slightly worse",
#                                           "No change",
#                                           "Slightly better",
#                                           "Significantly better"),
#                                         levels = c("Significantly worse",
#                                                    "Slightly worse",
#                                                    "No change",
#                                                    "Slightly better",
#                                                    "Significantly better")),
#                          n=c(2, 3, 4, 5, 6))
#
#   expect_equal(got, expected)
# })
#
# test_that("Validation checks work", {
#
#   dummy_data <- data.frame(Test = c("test1", "test2"))
#
#   expect_error(summarise_ability_change(dummy_data), "unexpected_input: no column called 'coding_ability_change'")
#
# })
