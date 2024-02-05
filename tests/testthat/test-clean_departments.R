
test_that("clean_departments output is as expected", {

  dummy_data <- data.frame(department = c(NA,
                                          "test",
                                          "Foreign, Commonwealth & Development Office (excl. agencies)",
                                          "Department for Environment, Food and Rural Affairs (excl. agencies)",
                                          "Forestry Commission",
                                          "Forest Research",
                                          "Forestry England",
                                          "Animal and Plant Health Agency",
                                          "Centre for Environment, Fisheries and Aquaculture Science",
                                          "Rural Payments Agency",
                                          "Environment Agency",
                                          "Marine Management Organisation",
                                          "Natural England"),
                           workplace = c(NA, "NHS", rep("test", 11)))

  got <- clean_departments(dummy_data)

  expected <- data.frame(department = c(NA,
                                        "NHS",
                                        "Foreign, Commonwealth and Development Office (excl. agencies)",
                                        "Department for Environment, Food and Rural Affairs (excl. agencies)",
                                        "Forestry Commission",
                                        "Forest Research",
                                        "Forestry England",
                                        "Animal and Plant Health Agency",
                                        "Centre for Environment, Fisheries and Aquaculture Science",
                                        "Rural Payments Agency",
                                        "Environment Agency",
                                        "Marine Management Organisation",
                                        "Natural England"),
                         workplace = c(NA, "NHS", rep("test", 11)),
                         defra = c(rep(FALSE, 3), rep(TRUE, 10)))

  expect_equal(got, expected)

})
