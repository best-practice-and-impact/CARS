
n=192
dummy_data <- data.frame(year = rep(c(2020, 2021, 2022), each=n/3),
                         code_freq = rep(c("Never", "Sometimes"), each=n/6, times=n/32),
                         knowledge_python = rep(c("Yes", "No"), each=n/12, times=n/16),
                         knowledge_R = rep(c("Yes", "No"), each=n/24, times=n/8),
                         knowledge_SAS = rep(c("Yes", "No"), each=n/48, times=n/4),
                         knowledge_SPSS = rep(c("Yes", "No"), each=n/96, times=n/2),
                         knowledge_stata = rep(c("Yes", "No"), each=n/192, times=n))

test_that("summarise_os_vs_prop missing data is handled correctly", {

  dummy_data[1,2] <- NA

  got <- summarise_os_vs_prop(dummy_data)

  expect_false(any(is.na.data.frame(got)))

})

test_that("summarise_os_vs_prop output is as expected", {

  got <- summarise_os_vs_prop(dummy_data)

  expected <- data.frame(lang_type = factor(rep(c("Open Source", "Proprietary"), each=3),
                                            levels = c("Open Source", "Proprietary")),
                         year = rep(c("2020", "2021", "2022"), times=2),
                         Freq = rep(c(48, 56), each=3),
                         n = rep(64, times=6)) %>%
    get_ci(freq_col = 3, n_col = 4)

  expect_equal(got, expected)

})
