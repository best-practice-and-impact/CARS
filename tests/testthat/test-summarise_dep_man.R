
dummy_data <- data.frame(dep_management = c(NA,
                                            rep("Yes", 2),
                                            rep("No", 3),
                                            rep("I don't know what dependency management is", 4)))

test_that("summarise_dep_man validation works", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_dep_man(dummy_data), "unexpected_input: no column called 'dep_management'")

})

test_that("summarise_dep_man missing data is handled correctly", {

  got <- summarise_dep_man(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_dep_man output is as expected", {

  got <- summarise_dep_man(dummy_data)

  expected <- data.frame(value = factor(c("Yes",
                                          "No",
                                          "I don't know what dependency management is"),
                                        levels = c("Yes",
                                                   "No",
                                                   "I don't know what dependency management is")),
                         n = c(0.22, 0.33, 0.44))

  expect_equal(got, expected)

})
