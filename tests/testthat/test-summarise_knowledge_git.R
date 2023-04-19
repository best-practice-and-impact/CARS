
dummy_data <- data.frame(knowledge_git = c(NA,
                                           rep("Yes", 2),
                                           rep("No", 3),
                                           rep("I don't know", 4)))

test_that("summarise_knowledge_git validation works", {

  dummy_data <- data.frame(Test = c("test1", "test2"))

  expect_error(summarise_knowledge_git(dummy_data), "unexpected_input: no column called 'knowledge_git'")

})

test_that("summarise_knowledge_git missing data is handled correctly", {

  got <- summarise_knowledge_git(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_knowledge_git output is as expected", {

  got <- summarise_knowledge_git(dummy_data)

  expected <- data.frame(value = factor(c("Yes",
                                          "No",
                                          "I don't know"),
                                        levels = c("Yes",
                                                   "No",
                                                   "I don't know")),
                         n = c(2/9, 1/3, 4/9))

  expect_equal(got, expected)

})
