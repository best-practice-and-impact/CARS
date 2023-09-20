
dummy_data <- data.frame(year = rep(c(2020, 2021, 2022), each=32),
                         knowledge_python = rep(rep(c("Yes", "No"), each=16), times=3),
                         knowledge_R = rep(rep(c("Yes", "No"), each=8), times=6),
                         knowledge_SAS = rep(rep(c("Yes", "No"), each=4), times=12),
                         knowledge_SPSS = rep(rep(c("Yes", "No"), each=2), times=24),
                         knowledge_stata = rep(c("Yes", "No"), times=48))

test_that("summarise_os_vs_prop missing data is handled correctly", {

  dummy_data[1,2] <- NA

  got <- summarise_os_vs_prop(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_os_vs_prop output is as expected", {

  got <- summarise_os_vs_prop(dummy_data)

  expected <- data.frame(lang_type = factor(rep(c("open source", "proprietary"), each=3),
                                            levels = c("open source", "proprietary")),
                         year = rep(c("2020", "2021", "2022"), times=2),
                         Freq = rep(c(24, 28), each=3),
                         n = rep(32, times=6)) %>%
    get_ci(freq_col = 3, n_col = 4)

  expect_equal(got, expected)

})
