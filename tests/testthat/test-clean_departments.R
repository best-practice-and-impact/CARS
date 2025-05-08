
test_that("clean_departments output is as expected", {

  dummy_data <- data.frame(department = c(NA,
                                          "test",
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
                           workplace = c(NA, "NHS or local healthcare service", rep("test", 10)))

  got <- clean_departments(dummy_data)

  expected <- data.frame(department = c(NA,
                                        "NHS",
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
                         workplace = c(NA, "NHS or local healthcare service", rep("test", 10)),
                         defra = c(rep(FALSE, 2), rep(TRUE, 10)))

  expect_equal(got, expected)

})
