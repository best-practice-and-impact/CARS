
test_that("question 1 routing removes redundant enteries in questions 2 to 5", {

  dummy_data <- data.frame(Q1. = c("Civil service, including desolved administations", "NHS", "Other"),
                           Q2. = c("test1", "test2", "test3"),
                           Q3. = c("test1", "test2", "test3"),
                           Q4.1 = c("test1", "test2", "test3"),
                           Q4.2 = c("test1", "test2", "test3"),
                           Q5. = c("test1", "test2", "test3")
  )

  skipped_questions = c(list("Q2.", "Q3.", "Q5."), grep("Q4.", colnames(dummy_data)))

  got <- check_routing_all(data = dummy_data,
                           question = "Q1.",
                           skipped_questions = skipped_questions,
                           condition = "Civil service, including desolved administations",
                           inverse = TRUE)

  exp <- data.frame(Q1. = c("Civil service, including desolved administations", "NHS", "Other"),
                    Q2. = c("test1", NA, NA),
                    Q3. = c("test1", NA, NA),
                    Q4.1 = c("test1", NA, NA),
                    Q4.2 = c("test1", NA, NA),
                    Q5. = c("test1", NA, NA)
  )

  expect_equal(got, exp)

})


test_that("question 3 routing removes redundant enteryin question 5", {

  dummy_data <- data.frame(Q3. = c("Office for National Statistics", "Other1", "Other2"),
                           Q4.1. = c("test1", "test2", "test3"),
                           Q4.2. = c("test1", "test2", "test3"),
                           Q5. = c("test1", "test2", "test3")
  )

  skipped_questions = list("Q5.")

  got <- check_routing_all(data = dummy_data,
                           question = "Q3.",
                           skipped_questions = skipped_questions,
                           condition = "Office for National Statistics",
                           inverse = TRUE)

  exp <- data.frame(Q3. = c("Office for National Statistics", "Other1", "Other2"),
                    Q4.1. = c("test1", "test2", "test3"),
                    Q4.2. = c("test1", "test2", "test3"),
                    Q5. = c("test1", NA, NA)
  )

  expect_equal(got, exp)

})


test_that("question 6 routing removes redundant enteries in questions 7 to 15", {

  dummy_data <- data.frame(Q6. = c("Any other qualification", "Other1", "Other2"),
                           Q7. = c("test1", "test2", "test3"),
                           Q8. = c("test1", "test2", "test3"),
                           Q9. = c("test1", "test2", "test3"),
                           Q10. = c("test1", "test2", "test3"),
                           Q11. = c("test1", "test2", "test3"),
                           Q12. = c("test1", "test2", "test3"),
                           Q13. = c("test1", "test2", "test3"),
                           Q14. = c("test1", "test2", "test3"),
                           Q15. = c("test1", "test2", "test3")
  )

  skipped_questions = list("Q7.", "Q8.", "Q9.", "Q10.", "Q11.", "Q12.", "Q13.", "Q14.", "Q15.")

  got <- check_routing_all(data = dummy_data,
                           question = "Q6.",
                           skipped_questions = skipped_questions,
                           condition = "Any other qualification")

  exp <- data.frame(Q6. = c("Any other qualification", "Other1", "Other2"),
                    Q7. = c(NA, "test2", "test3"),
                    Q8. = c(NA, "test2", "test3"),
                    Q9. = c(NA, "test2", "test3"),
                    Q10. = c(NA, "test2", "test3"),
                    Q11. = c(NA, "test2", "test3"),
                    Q12. = c(NA, "test2", "test3"),
                    Q13. = c(NA, "test2", "test3"),
                    Q14. = c(NA, "test2", "test3"),
                    Q15. = c(NA, "test2", "test3")
  )

  expect_equal(got, exp)

})


test_that("question 16 routing removes redundant enteries in questions 30 to 35", {

  dummy_data <- data.frame(Q16. = c("Never", "Sometimes", "All the time"),
                           Q30. = c("test1", "test2", "test3"),
                           Q31. = c("test1", "test2", "test3"),
                           Q32. = c("test1", "test2", "test3"),
                           Q33. = c("test1", "test2", "test3"),
                           Q34. = c("test1", "test2", "test3"),
                           Q35. = c("test1", "test2", "test3")
  )

  skipped_questions = list("Q30.", "Q31.", "Q32.", "Q33.", "Q34.", "Q35.")

  got <- check_routing_all(data = dummy_data,
                           question = "Q16.",
                           skipped_questions = skipped_questions,
                           condition = "Never")

  exp <- data.frame(Q16. = c("Never", "Sometimes", "All the time"),
                    Q30. = c(NA, "test2", "test3"),
                    Q31. = c(NA, "test2", "test3"),
                    Q32. = c(NA, "test2", "test3"),
                    Q33. = c(NA, "test2", "test3"),
                    Q34. = c(NA, "test2", "test3"),
                    Q35. = c(NA, "test2", "test3")
  )

  expect_equal(got, exp)

})


test_that("question 22 routing removes redundant enteries in questions 23 to 25", {

  dummy_data <- data.frame(Q22. = c("No", "Yes"),
                           Q23. = c("test1", "test2"),
                           Q24. = c("test1", "test2"),
                           Q25. = c("test1", "test2")
  )

  skipped_questions = list("Q23.", "Q24.", "Q25.")

  got <- check_routing_all(data = dummy_data,
                           question = "Q22.",
                           skipped_questions = skipped_questions,
                           condition = "No")

  exp <- data.frame(Q22. = c("No", "Yes"),
                    Q23. = c(NA, "test2"),
                    Q24. = c(NA, "test2"),
                    Q25. = c(NA, "test2")
  )

  expect_equal(got, exp)

})


test_that("question 24 routing removes redundant entery in question 25", {

  dummy_data <- data.frame(Q24. = c("No", "Yes"),
                           Q25. = c("test1", "test2")
  )

  skipped_questions = list("Q25.")

  got <- check_routing_all(data = dummy_data,
                           question = "Q24.",
                           skipped_questions = skipped_questions,
                           condition = "No")

  exp <- data.frame(Q24. = c("No", "Yes"),
                    Q25. = c(NA, "test2")
  )

  expect_equal(got, exp)

})


test_that("question 26 routing removes redundant enteries in questions 27 to 29", {

  dummy_data <- data.frame(Q26. = c("No", "Yes"),
                           Q27. = c("test1", "test2"),
                           Q28. = c("test1", "test2"),
                           Q29.1. = c("test1", "test2"),
                           Q29.2. = c("test1", "test2")
  )

  skipped_questions = c(list("Q27.", "Q28."), grep("Q29.", colnames(dummy_data)))

  got <- check_routing_all(data = dummy_data,
                           question = "Q26.",
                           skipped_questions = skipped_questions,
                           condition = "No")

  exp <- data.frame(Q26. = c("No", "Yes"),
                    Q27. = c(NA, "test2"),
                    Q28. = c(NA, "test2"),
                    Q29.1. = c(NA, "test2"),
                    Q29.2. = c(NA, "test2")
  )

  expect_equal(got, exp)

})
