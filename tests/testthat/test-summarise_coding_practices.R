
answers <- c("I don't understand this question", "Never", "Rarely", "Sometimes", "Regularly", "All the time")

dummy_data <- data.frame(prac_use_open_source = answers,
                         prac_open_source_own = answers,
                         prac_version_control = answers,
                         prac_review = answers,
                         prac_functions = answers,
                         prac_unit_test = answers,
                         prac_package = answers,
                         prac_dir_structure = answers,
                         prac_style = answers,
                         prac_automated_QA = answers,
                         prac_development_QA = answers,
                         prac_proportionate_QA = answers)

test_that("summarise_coding_practises missing data is handled correctly", {

  dummy_data[1,1] <- NA

  got <- summarise_coding_practices(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_coding_practises output is as expected", {

  got <- summarise_coding_practices(dummy_data)

  expected <- data.frame(name = rep(sort(c("Use open source software",
                                           "Open source own code",
                                           "Version control",
                                           "Code review",
                                           "Functions",
                                           "Unit testing",
                                           "Packaging code",
                                           "Standard directory structure",
                                           "Coding guidelines / Style guides",
                                           "Automated data quality assurance",
                                           "Quality assurance throughout development",
                                           "Proportionate quality assurance")), each=6),
                         value = factor(rep(answers, 12),
                                        levels = answers),
                         n = rep(1/6, times=72))

  expect_equal(got, expected)

})
