
test_that("Output is as expected", {

  dummy_data <- data.frame(code_freq = c(rep("Never", 8), "Sometimes", "Regularly"),
                           prev_coding_experience = c(rep("Yes", 8), NA, "No"),
                           first_learned = c(rep("Self-taught" , 3),
                                             rep( "In public sector employment", 3),
                                             rep("other" , 1),
                                             rep(NA , 3)))

  got <- summarise_where_learned_code(dummy_data)

  expected <- data.frame(name = c(rep("First coding experience", 6)),
                         value = factor(c("In current role",
                                          "In education",
                                          "In private sector employment",
                                          "In public sector employment",
                                          "Self-taught",
                                          "Other"),
                                        levels = c("In current role",
                                                   "In education",
                                                   "In private sector employment",
                                                   "In public sector employment",
                                                   "Self-taught",
                                                   "Other")),
                         n = c(2, 0, 0, 3, 3, 1))

  expect_equal(got, expected)

})

test_that("Validation checks work", {

  dummy_data_1 <- data.frame(code_freq = c(rep("Never", 8), "Sometimes", "Regularly"),
                             prev_coding_experience = c(rep("Yes", 8), NA, "No"))

  expect_error(summarise_where_learned_code(dummy_data_1), "unexpected_input: no column called 'first_learned'")

  dummy_data_2 <- data.frame(prev_coding_experience = c(rep("Yes", 8), NA, "No"),
                             first_learned = c(rep("Self-taught" , 3),
                                               rep( "In public sector employment", 3),
                                               rep("other" , 1),
                                               rep(NA , 3)))

  expect_error(summarise_where_learned_code(dummy_data_2), "unexpected_input: no column called 'code_freq'")

  dummy_data_3 <- data.frame(code_freq = c(rep("Never", 8), "Sometimes", "Regularly"),
                             first_learned = c(rep("Self-taught" , 3),
                                               rep( "In public sector employment", 3),
                                               rep("other" , 1),
                                               rep(NA , 3)))

  expect_error(summarise_where_learned_code(dummy_data_3), "unexpected_input: no column called 'prev_coding_experience'")

})
