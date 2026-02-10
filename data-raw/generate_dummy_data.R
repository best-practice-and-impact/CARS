## code to prepare `generate_dummy_data` dataset goes here

library(dplyr)
library(purrr)
library(usethis)

sample_replace <- purrr::partial(sample, size = 502, replace = TRUE)

sample_replace_500 <- purrr::partial(sample, size = 500, replace = TRUE)

config <- yaml::read_yaml("./config.yml")

create_dummy_data <- function(type = c("test", "clean")){

  set.seed(555)

  cars_dummy_data <- dplyr::tibble(UserID = c(NA,
                                       NA,
                                       100000000:100000499),
                            UserNo = c(NA,
                                       NA,
                                       1:500),
                            Name = c(NA),
                            Email = c(NA),
                            `IP Address` = c(NA,
                                             NA,
                                             rep("100.00.000.00", 500)),
                            `Unique ID` = c(NA),
                            Started = c(NA,
                                        NA,
                                        sample(seq(as.POSIXct('2023/10/16'),
                                                   as.POSIXct('2023/11/16'),
                                                   by="15 mins"),
                                               500)),
                            Ended = c(NA,
                                      NA,
                                      sample(seq(as.POSIXct('2023/10/16'),
                                                 as.POSIXct('2023/11/16'),
                                                 by="15 mins"),
                                             500)),
                            Tracking = c(NA,
                                         NA,
                                         sample(names(config[["tracking_link"]]),
                                                500,
                                                replace = TRUE)),
                            Q1 = c(NA,
                                   NA,
                                   sample(config[["workplace"]][["levels"]],
                                          500,
                                          prob = (c(0.8, 0.19, 0.01)),
                                          replace = TRUE))) %>%
    dplyr::mutate(Q2 = dplyr::case_when(Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["cs_grade"]][["levels"]]),
                          TRUE ~ NA),
           Q3 = dplyr::case_when(dplyr::row_number() == 1 ~ "Answer",
                          Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["department"]][["levels"]]),
                          TRUE ~ NA),
           Q3.1 = dplyr::case_when(Q3 == NA ~ "some text",
                            Q3 == "Answer" ~ "Other (please specify)",
                            TRUE ~ NA),
           Q4.1 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.1. Data Engineer (any profession)",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.2 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.2. Data Scientist (any profession)",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.3 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.3. Digital, Data and Technology Profession",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.4 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.4. Government Actuary's Department",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.5 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.5. Government Economic Service",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.6 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.6. Government Geography Profession",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.7 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.7. Government Operational Research Service",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.8 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.8. Government Science & Engineering",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.9 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.9. Government Social Research",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.10 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.10. Government Statistician Group",
                             Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.11 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.11. Civil Service, no profession membership",
                             Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q4.12 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q4.12. Other Civil Service profession",
                             Q1 == "Civil service, including devolved administrations" ~ sample_replace(config[["professions"]][["levels"]])),
           Q5 = dplyr::if_else(Q3 == "Office for National Statistics",
                        sample_replace(config[["ons_directorate"]][["levels"]]),
                        NA),
           Q6 = dplyr::if_else(Q1 == "NHS or local healthcare service",
                        sample_replace(config[["pay_band"]][["levels"]], prob = c(0.9, 0.09, 0.01)),
                        NA),
           Q7 = dplyr::if_else(Q6 == "NHS",sample_replace(config[["nhs_band"]][["levels"]]),
                        NA),
           Q8 = dplyr::if_else(Q6 == "Local Authority or NJC",
                        sample_replace(config[["njc_grade"]][["levels"]]),
                        NA),
           Q9 = c(NA, NA, sample(config[["time_in_role"]][["levels"]], 500, replace = TRUE)),
           Q10 = c(NA, NA, sample(config[["coding_exp"]][["levels"]], 500, replace = TRUE)),
           Q11 = c(NA, NA, sample(config[["team"]][["levels"]], 500, replace = TRUE)),
           Q12 = c(NA, NA, sample(config[["manage_project"]][["levels"]], 500, replace = TRUE)),
           Q13 = dplyr::if_else(Q10 == "No",
                         sample_replace(config[["coding_learn_pref"]][["levels"]]),
                         NA))

  Q14 <- paste0("Q", seq(14.1, 14.7, 0.1))
  cars_dummy_data[, Q14] <- NA

  cars_dummy_data <- cars_dummy_data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(Q14)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" ~ sample_replace(config[["coding_learn_barriers"]][["levels"]]),
                                                                          .default = NA)),
                  Q14.8 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q14.8. Other (please specify):",
                                           Q10 != "No" ~ sample_replace(c(NA, "some text"), prob = c(0.8, 0.2))),
                  Q15 = dplyr::if_else(Q10 != "No",
                                       sample_replace(config[["coding_years"]][["levels"]]),
                                       NA),
                  Q16 = dplyr::if_else(Q10 != "No",
                                       sample_replace(config[["code_freq"]][["levels"]]),
                                       NA),
                  Q17 = dplyr::if_else(Q10 != "No",
                                       sample_replace(config[["coding_freq_pref"]][["levels"]]),
                                       NA),
                  Q18 = dplyr::if_else(Q10 != "No",
                                       sample_replace(config[["ability_change"]][["levels"]]),
                                       NA)
                  )

  Q19 <- paste0("Q", seq(19.1, 19.7, 0.1), " ", config[["coding_learn_barriers"]][["cols"]][-8])
  Q20 <- paste0("Q", seq(20.1, 20.11, 0.1), " ", config[["coding_tools_knowledge"]][["cols"]][-11])
  Q21 <- paste0("Q", seq(21.1, 21.11, 0.1), " ", config[["coding_tools_use"]][["cols"]][-11])
  Q23 <- paste0("Q", seq(23.1, 23.6, 0.1), " ", config[["cloud"]][["cols"]][-6] )
  Q25 <- paste0("Q", seq(25.1, 25.6, 0.1), " ", config[["ai_tools"]][["cols"]][-6])
  Q26 <- paste0("Q", seq(26.1, 26.5, 0.1), " ", config[["ai_use"]][["cols"]][-5])
  Q28 <- paste0("Q", seq(28.1, 28.8, 0.1), " ", config[["coding_practices"]][["cols"]])
  Q29 <- paste0("Q", seq(29.1, 29.6, 0.1), " ", config[["working_practices"]][["cols"]])
  Q30 <- paste0("Q", seq(30.1, 30.5, 0.1), " ", config[["doc"]][["cols"]])
  Q32 <- paste0("Q", seq(32.1, 32.8, 0.1), " ", config[["rap_barriers"]][["cols"]][-9])

  cars_dummy_data[, c(Q19, "Q19.8", Q20, "Q20.12",
                      Q21, "Q21.12", "Q22", Q23, "Q23.7",
                      "Q24", Q25, Q26, "Q27", Q28, Q29,
                      Q30, "Q31", Q32)] <- NA

  cars_dummy_data <- cars_dummy_data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(Q19)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" ~ sample_replace(config[["coding_learn_barriers"]][["levels"]]),
                                                                          .default = NA)),
                  Q19.8 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q19.8. Other (please specify):",
                                           Q10 != "No" ~ sample_replace(c(NA, "some text"), prob = c(0.8, 0.2))),
                  dplyr::across(dplyr::all_of(c(Q20)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" ~ sample_replace(config[["coding_tools_knowledge"]][["levels"]]),
                                                                          .default = NA)),
                  Q20.12 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q20.12. Other (please specify)",
                                            Q10 != "No" ~ sample_replace(c(NA, "some text"), prob = c(0.8, 0.2))),
                  dplyr::across(dplyr::all_of(c(Q21)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" ~ sample_replace(config[["coding_tools_use"]][["levels"]]),
                                                                          .default = NA)),
                  Q21.12 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q21.12. Other (please specify)",
                                            Q10 != "No" ~ sample_replace(c(NA, "some text"), prob = c(0.8, 0.2))),
                  Q22 = dplyr::if_else(Q10 != "No",
                                       sample_replace(config[["git"]][["levels"]]),
                                       NA),
                  dplyr::across(dplyr::all_of(c(Q23)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" ~ sample_replace(config[["cloud"]][["levels"]]),
                                                                          .default = NA)),
                  Q23.7 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q23.7. Other (please specify)",
                                           Q10 != "No" ~ sample_replace(c(NA, "some text"), prob = c(0.8, 0.2))),
                  Q24 = dplyr::if_else(Q10 != "No" & Q16 != "Never",
                                       sample_replace(config[["ai"]][["levels"]]),
                                       NA),
                  dplyr::across(dplyr::all_of(c(Q25)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" & Q16 != "Never" & Q24 != "No" ~ sample_replace(config[["ai_tools"]][["levels"]]),
                                                                          .default = NA)),
                  Q25.7 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q25.7. Other (please specify)",
                                           Q10 != "No" & Q16 != "Never" & Q24 != "No" ~ sample_replace(c(NA, "some text"), prob = c(0.8, 0.2))),
                  dplyr::across(dplyr::all_of(c(Q26)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" ~ sample_replace(config[["ai_use"]][["levels"]]),
                                                                          .default = NA)),
                  Q27 = dplyr::if_else(Q10 != "No" & Q16 != "Never" & Q24 != "No",
                                       sample_replace(config[["git"]][["levels"]]),
                                       NA),
                  dplyr::across(dplyr::all_of(c(Q28)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" & Q16 != "Never" ~ sample_replace(config[["coding_practices"]][["levels"]]),
                                                                          .default = NA)),
                  dplyr::across(dplyr::all_of(c(Q29)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" & Q16 != "Never" ~ sample_replace(config[["working_practices"]][["levels"]]),
                                                                          .default = NA)),
                  dplyr::across(dplyr::all_of(c(Q30)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" & Q16 != "Never" ~ sample_replace(config[["doc"]][["levels"]]),
                                                                          .default = NA)),
                  Q31 = dplyr::if_else(Q10 != "No",
                                       sample_replace(config[["rap_implementing"]][["levels"]]),
                                       NA),
                  dplyr::across(dplyr::all_of(c(Q32)), ~ dplyr::case_when(dplyr::row_number() == 1 ~ dplyr::cur_column(),
                                                                          Q10 != "No" & Q16 != "Never" ~ sample_replace(config[["rap_barriers"]][["levels"]]),
                                                                          .default = NA)),
                  Q32.9 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q32.9. Other (please specify)",
                                           Q10 != "No"~ sample_replace(c(NA, "some text"), prob = c(0.8, 0.2))),
                  Q32.10 = dplyr::case_when(dplyr::row_number() == 1 ~ "Q32.10. Additional info",
                                           Q10 != "No" ~ sample_replace(c(NA, "some text"), prob = c(0.8, 0.2))),
                  Q33 = dplyr::if_else(Q10 != "No",
                                       sample_replace(config[["standards"]][["levels"]]),
                                       NA),
                  Q34 = dplyr::if_else(Q10 != "No",
                                       sample_replace(config[["duck_book"]][["levels"]]),
                                       NA),
                  Q35 = dplyr::if_else(Q10 != "No",
                                       sample_replace(config[["packages"]][["levels"]]),
                                       NA),
                  Q36 = dplyr::if_else(Q10 != "No",
                                       sample_replace(c(NA, "some text"), prob = c(0.8, 0.2)),
                                       NA),
                  Q37 = if_else(Q3 == "Office for National Statistics",
                                sample_replace(config[["qs_aware"]][["levels"]]),
                                NA),
                  Q38 = if_else(Q3 == "Office for National Statistics",
                                sample_replace(config[["qs_comply"]][["levels"]]),
                                NA),
                  Q39 = if_else(Q3 == "Office for National Statistics",
                                sample_replace(config[["qq_aware"]][["levels"]]),
                                NA),
                  Q40 = sample_replace(c(NA, "some text"), prob = c(0.8, 0.2)),
                  Q41 = sample_replace(c(NA, "some text"), prob = c(0.8, 0.2))

    )


  if(type == "test"){
    cars_dummy_data <- cars_dummy_data |>
      dplyr::mutate(dplyr::across(c(Q1:Q3,
                                    Q4.1:Q8,
                                    Q13:Q14.7,
                                    Q15:Q18,
                                    Q22,
                                    Q24,
                                    Q27,
                                    Q31,
                                    Q33:Q39), ~ dplyr::case_when(dplyr::row_number() %in% c(3:500) ~ tidyr::replace_na(., "test"),
                                                                                 TRUE ~ .)))|>
      dplyr::mutate(dplyr::across(dplyr::all_of(c(Q19,
                                                  Q20,
                                                  Q21,
                                                  Q23,
                                                  Q25,
                                                  Q26,
                                                  Q28,
                                                  Q29,
                                                  Q30,
                                                  Q32)), ~ dplyr::case_when(dplyr::row_number() %in% c(3:500) ~ tidyr::replace_na(., "test"),
                                                                            TRUE ~ .)))
  }

  return(cars_dummy_data)
}


cars_dummy_data_test <- create_dummy_data(type = "test")
cars_dummy_data_clean <- create_dummy_data(type = "clean")

usethis::use_data(cars_dummy_data_test, cars_dummy_data_clean, config, internal = TRUE, overwrite = TRUE)


