
test_that("clean_departments output is as expected", {

  dummy_data <- data.frame(department = c(NA,
                                          "test",
                                          "test",
                                          "Department for Environment, Food and Rural Affairs (excl. agencies)",
                                          "Animal and Plant Health Agency",
                                          "Centre for Environment, Fisheries and Aquaculture Science",
                                          "Rural Payments Agency",
                                          "Environment Agency",
                                          "Marine Management Organisation",
                                          "Natural England"),
                           other_department_name = c(NA, "Forest research", rep("test", 8)),
                           workplace = c(NA, "test", "NHS", rep("test", 7)))

  got <- clean_departments(dummy_data)

  expected <- data.frame(department = c(NA,
                                        "Forestry Commission",
                                        "NHS",
                                        "Department for Environment, Food and Rural Affairs (excl. agencies)",
                                        "Animal and Plant Health Agency",
                                        "Centre for Environment, Fisheries and Aquaculture Science",
                                        "Rural Payments Agency",
                                        "Environment Agency",
                                        "Marine Management Organisation",
                                        "Natural England"),
                         other_department_name = c(NA, "Forest research", rep("test", 8)),
                         workplace = c(NA, "test", "NHS", rep("test", 7)),
                         defra = c(FALSE, TRUE, FALSE, rep(TRUE, 7)))

  expect_equal(got, expected)

})
