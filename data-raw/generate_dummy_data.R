## code to prepare `generate_dummy_data` dataset goes here

library(dplyr)
library(purrr)
library(usethis)

sample_replace <- purrr::partial(sample, size = 502, replace = TRUE)

sample_replace_500 <- purrr::partial(sample, size = 500, replace = TRUE)

tracking_list = c("Default Web Link",
                  "Web Link 2",
                  "Web link 3",
                  "Web link 4",
                  "Web link 5")

q1_options = c("Civil service, including devolved administrations",
               "NHS or local healthcare service",
               "Other")

q2_options = c("SCS Pay Band 1 (or equivalent)",
               "Grade 6 (or equivalent)",
               "Grade 7 (or equivalent)",
               "Senior Executive Officer (or equivalent)",
               "Higher Executive Officer (or equivalent)",
               "Executive Officer (or equivalent)",
               "Administrative Officer (or equivalent)",
               "Fast Stream",
               "Other")

q3_options = c( "Attorney General's Office",
                "Cabinet Office (excl. agencies)",
                "Department for Business & Trade (excl. agencies)",
                "Department for Culture, Media & Sport",
                "Department for Education (excl. agencies)",
                "Department for Energy Security & Net Zero",
                "Department for Environment Food & Rural Affairs (excl. agencies)",
                "Department for Levelling Up, Housing & Communities (excl. agencies)",
                "Department for Science, Innovation & Technology (excl. agencies)",
                "Department for Transport (excl. agencies)",
                "Department for Work & Pensions",
                "Department of Health & Social Care (excl. agencies)",
                "Foreign, Commonwealth & Development Office (excl. agencies)",
                "HM Treasury (excl. agencies)",
                "Home Office",
                "Ministry of Defence (excl. agencies)",
                "Ministry of Justice (excl. agencies)",
                "Northern Ireland Office",
                "Office of the Advocate General for Scotland",
                "Office of the Leader of the House of Commons",
                "Office of the Leader of the House of Lords",
                "Office of the Secretary of State for Scotland",
                "Office of the Secretary of State for Wales",
                "UK Export Finance",
                "The Charity Commission",
                "Competition and Markets Authority",
                "Crown Prosecution Service",
                "Food Standards Agency",
                "Forestry Commission (excl. agencies)",
                "Government Actuary's Department",
                "Government Legal Department",
                "HM Land Registry",
                "HM Revenue & Customs (excl. agencies)",
                "NS&I",
                "The National Archives",
                "National Crime Agency",
                "Office of Rail and Road",
                "Ofgem",
                "Ofqual",
                "Ofsted",
                "Serious Fraud Office",
                "Supreme Court of the United Kingdom",
                "UK Statistics Authority",
                "The Water Services Regulation Authority",
                "Prime Minister's Office, 10 Downing Street",
                "Crown Commercial Service",
                "Government Property Agency",
                "Companies House",
                "The Insolvency Service",
                "Building Digital UK",
                "Intellectual Property Office",
                "Met Office",
                "UK Space Agency",
                "Planning Inspectorate",
                "Queen Elizabeth II Conference Centre",
                "Education and Skills Funding Agency",
                "Teaching Regulation Agency",
                "Standards and Testing Agency",
                "Animal and Plant Health Agency",
                "Centre for Environment, Fisheries and Aquaculture Science",
                "Rural Payments Agency",
                "Veterinary Medicines Directorate",
                "Active Travel England",
                "Driver and Vehicle Licensing Agency",
                "Driver and Vehicle Standards Agency",
                "Maritime and Coastguard Agency",
                "Vehicle Certification Agency",
                "Medicines and Healthcare products Regulatory Agency",
                "UK Health Security Agency",
                "Wilton Park",
                "Government Internal Audit Agency",
                "National Infrastructure Commission",
                "UK Debt Management Office",
                "Defence Electronics and Components Agency",
                "Defence Equipment and Support",
                "Defence Science and Technology Laboratory",
                "UK Hydrographic Office",
                "Submarine Delivery Agency",
                "Criminal Injuries Compensation Authority",
                "HM Courts & Tribunals Service",
                "HM Prison and Probation Service",
                "Legal Aid Agency",
                "Office of the Public Guardian",
                "Forest Research",
                "Forestry England",
                "Valuation Office Agency",
                "Office for National Statistics",
                "Northern Ireland Executive",
                "Scottish Government (excl. agencies)",
                "Welsh Government",
                "Accountant in Bankruptcy",
                "Disclosure Scotland",
                "Education Scotland",
                "Forestry and Land Scotland",
                "Scottish Forestry",
                "Scottish Prison Service",
                "Scottish Public Pensions Agency",
                "Social Security Scotland",
                "Student Awards Agency for Scotland",
                "Transport Scotland",
                "-")



q5_options = c("Directorate 1",
               "Directorate 2",
               "Directorate 3",
               "Directorate 4",
               "Directorate 5",
               "Directorate 6")

q6_options = c("NHS",
               "Local Authority or NJC",
               "Other / Not sure")

q7_options = c("Band 1",
               "Band 2",
               "Band 3",
               "Band 4",
               "Band 5",
               "Band 6")

q9_options = c("England",
               "Scotland",
               "Wales",
               "Northern Ireland")

q10_options = c("NHS England",
                "NHS Trust",
                "Other")

q11_options = c("NHS Scotland",
                "Public Health Scotland",
                "Regional Health Board",
                "Special Health Board",
                "Other")

q12_options = c("Public Health Wales",
                "Local Health Board",
                "NHS Trust",
                "Other")

q13_options = c("Public Health Agency",
                "Health and Social Care Trust",
                "Other")

q14_options = c("Doctoral degree (or equivalent)",
                "Master's degree (or equivalent)",
                "Bachelor's degree (or equivalent)",
                "Any other qualification")

subject_options = c("English",
                    "Mathematics",
                    "Science",
                    "Other")

level_options = c("BSc",
                  "MSc",
                  "PhD",
                  "Other")


q25_options = c("Yes",
                "No - I manage people who do not write code",
                "No - I don't line manage anyone")


q31_options = c("Education",
                "Current employment",
                "Previous public sector employment",
                "Previous private sector employment",
                "Other: some text")

q32_options = c("It has become significantly better",
                "It has become slightly better",
                "It has stayed the same",
                "It has become slightly worse",
                "It has become significantly worse"
)

q35_options = c("Yes, and I am a RAP Champion",
                "Yes",
                "No")

q36_options = c("Yes",
                "Yes, but I haven't read it",
                "No")

q38_options = c("I don't understand this question",
                "Never",
                "Rarely",
                "Sometimes",
                "Regularly",
                "All the time")

yes_no = c("Yes",
           "No")

yes_no_dk = c("Yes",
              "No",
              "I don't know")

yes_no_notreq = c("Yes",
                  "No",
                  "Not required for my work")

likert = c("Strongly Disagree",
           "Disagree",
           "Neutral",
           "Agree",
           "Strongly Agree")

freq_options = c("Never",
                 "Rarely",
                 "Sometimes",
                 "Regularly",
                 "All the time")

create_dummy_data <- function(type = c("test", "clean")){

  set.seed(100)

  cars_dummy_data <- tibble(UserID = c(NA,
                                       NA,
                                       100000000:100000499),
                            UserNo = c(NA,
                                       NA,
                                       1:500),
                            Name = c(NA),
                            Email = c(NA),
                            IP = c(NA,
                                   NA,
                                   rep("100.00.000.00", 500)),
                            ID = c(NA),
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
                                         sample(tracking_list,
                                                500,
                                                replace = TRUE)),
                            Q1 = c(NA,
                                   NA,
                                   sample(q1_options,
                                          500,
                                          prob = (c(0.8, 0.19, 0.01)),
                                          replace = TRUE))) %>%
    mutate(Q2 = case_when(Q1 == "Civil service, including devolved administrations" ~ sample_replace(q2_options),
                          TRUE ~ NA),
           Q3 = case_when(row_number() == 1 ~ "Answer",
                          Q1 == "Civil service, including devolved administrations" ~ sample_replace(q3_options),
                          TRUE ~ NA),
           Q3.1 = case_when(Q3 == NA ~ "some text",
                            Q3 == "Answer" ~ "Other (please specify)",
                            TRUE ~ NA),
           Q4.1 = case_when(row_number() == 1 ~ "Q4.1. Data Engineer (any profession)",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q4.2 = case_when(row_number() == 1 ~ "Q4.2. Data Scientist (any profession)",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q4.3 = case_when(row_number() == 1 ~ "Q4.3. Digital, Data and Technology Profession",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q4.4 = case_when(row_number() == 1 ~ "Q4.4. Government Actuary's Department",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q4.5 = case_when(row_number() == 1 ~ "Q4.5. Government Economic Service",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q4.6 = case_when(row_number() == 1 ~ "Q4.6. Government Geography Profession",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q4.7 = case_when(row_number() == 1 ~ "Q4.7. Government Operational Research Service",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q4.8 = case_when(row_number() == 1 ~ "Q4.8. Government Social Research",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q4.9 = case_when(row_number() == 1 ~ "Q4.9. Government Statistician Group",
                            Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q4.10 = case_when(row_number() == 1 ~ "Q4.10. Civil Service, no profession membership",
                             Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q4.11 = case_when(row_number() == 1 ~ "Q4.11. Other Civil Service profession",
                             Q1 == "Civil service, including devolved administrations" ~ sample_replace(yes_no)),
           Q5 = case_when(Q3 == "Office for National Statistics" ~ sample_replace(q5_options)),
           Q6 = case_when(Q1 == "NHS or local healthcare service" ~ sample_replace(q6_options, prob = c(0.9, 0.09, 0.01)),
                          TRUE ~ NA),
           Q7 = case_when(Q6 == "NHS" ~ sample_replace(q7_options),
                          TRUE ~ NA),
           Q8 = case_when(Q6 == "Local Authority or NJC" ~ sample_replace(q7_options),
                          TRUE ~ NA),
           Q9 = case_when(Q1 == "NHS or local healthcare service" ~ sample_replace(q9_options),
                          TRUE ~ NA),
           Q10 = case_when(Q9 == "England"  ~ sample_replace(q10_options),
                           TRUE ~ NA),
           Q11 = case_when(Q9 == "Scotland"  ~ sample_replace(q11_options),
                           TRUE ~ NA),
           Q12 = case_when(Q9 == "Wales"  ~ sample_replace(q12_options),
                           TRUE ~ NA),
           Q13 = case_when(Q9 == "Northern Ireland"  ~ sample_replace(q13_options),
                           TRUE ~ NA),
           Q14 = c(NA, NA, sample(q14_options,
                                  500,
                                  replace = TRUE)),
           Q15 = case_when(Q14 != "Any other qualification"  ~ sample_replace(subject_options),
                           TRUE ~ NA),
           Q16 = case_when(!is.na(Q15)  ~ sample_replace(level_options),
                           TRUE ~ NA),
           Q17 = case_when(!is.na(Q15)  ~ sample_replace(yes_no),
                           TRUE ~ NA),
           Q18 = case_when(Q14 != "Any other qualification"  ~ sample_replace(c(subject_options, NA)),
                           TRUE ~ NA),
           Q19 = case_when(!is.na(Q18)  ~ sample_replace(level_options),
                           TRUE ~ NA),
           Q20 = case_when(!is.na(Q18)  ~ sample_replace(yes_no),
                           TRUE ~ NA),
           Q21 = case_when(Q14 != "Any other qualification" & !is.na(Q18) ~ sample_replace(c(subject_options, NA)),
                           TRUE ~ NA),
           Q22 = case_when(!is.na(Q21)  ~ sample_replace(level_options),
                           TRUE ~ NA),
           Q23 = case_when(!is.na(Q21)  ~ sample_replace(yes_no),
                           TRUE ~ NA),
           Q24 = c(NA, NA, sample(freq_options,
                                  500,
                                  replace = TRUE)),
           Q25 = c(NA, NA, sample(q25_options,
                                  500,
                                  replace = TRUE)),
           Q26.1 = c("Q26.1. Matlab",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q26.2 = c("Q26.2. Python",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q26.3 = c("Q26.3. R",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q26.4 = c("Q26.4. SAS",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q26.5 = c("Q26.5. SPSS",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q26.6 = c("Q26.6. SQL",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q26.7 = c("Q26.7. Stata",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q26.8 = c("Q26.8. VBA",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q26.9 = c("Q26.9. Other open source tool (please specify)",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q26.10 = c("Q26.10. Other license/closed source tool (please specify)",
                      NA,
                      sample_replace_500(yes_no_dk)),
           Q26.11 = c("Q26.11. Other (please specify the tool and if it available to use for your work):",
                      NA, sample_replace_500(c(NA,
                                               "some text"),
                                             prob = c(0.8, 0.2))),
           Q27.1 = c("Q27.1. Matlab",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q27.2 = c("Q27.2. Python",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q27.3 = c("Q27.3. R",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q27.4 = c("Q27.4. SAS",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q27.5 = c("Q27.5. SPSS",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q27.6 = c("Q27.6. SQL",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q27.7 = c("Q27.7. Stata",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q27.8 = c("Q27.8. VBA",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q27.9 = c("Q27.9. Other open source tool (please specify)",
                     NA,
                     sample_replace_500(yes_no_dk)),
           Q27.10 = c("Q27.10. Other license/closed source tool (please specify)",
                      NA,
                      sample_replace_500(yes_no_dk)),
           Q27.11 = c("Q27.11. Other (please specify the tool and if it available to use for your work):",
                      NA,
                      sample_replace_500(c(NA,
                                           "some text"),
                                         prob = c(0.8, 0.2))),
           Q28 = c(NA, NA, sample_replace_500(yes_no_dk)),
           Q29 = c(NA, NA, sample_replace_500(yes_no_dk)),
           Q30 = case_when(Q24 != "Never" ~ sample_replace(yes_no)),
           Q31 = case_when(Q24 != "Never" & Q30 == "Yes" ~ sample_replace(q31_options)),
           Q32 = case_when(Q24 != "Never" & Q30 == "Yes" ~ sample_replace(q32_options)),
           Q33 = case_when(Q24 != "Never" ~ sample_replace(yes_no)),
           Q34 = case_when(Q33 != "No" ~ sample_replace(yes_no_dk)),
           Q35 = case_when(Q34 == "Yes" ~ sample_replace(q35_options)),
           Q36 = case_when(Q33 != "No" ~ sample_replace(q36_options)))

  q37 <- paste0("Q", seq(37.1, 37.8, 0.1))
  cars_dummy_data[, q37] <- NA

  q38 <- paste0("Q", seq(38.01, 38.13, 0.01))
  cars_dummy_data[, q38] <- NA

  q39 <- paste0("Q", seq(39.1, 39.8, 0.1))
  cars_dummy_data[, q39] <- NA

  q40 <- paste0("Q", 40:42)
  cars_dummy_data[, q40] <- NA

  cars_dummy_data <- cars_dummy_data %>% mutate(across(all_of(q37),
                                                       ~ case_when(Q24 != "Never" & Q33 == "Yes" ~ sample_replace(likert),
                                                                   TRUE ~ NA))
  ) %>%
    mutate(across(all_of(q38),
                  ~ case_when(Q24 != "Never" ~ sample_replace(q38_options),
                              TRUE ~ NA))
    ) %>%
    mutate(across(all_of(q39),
                  ~ case_when(Q24 != "Never" ~ sample_replace(q38_options),
                              TRUE ~ NA))
    ) %>%
    mutate(across(all_of(q40),
                  ~ case_when(Q24 != "Never" ~ sample_replace(yes_no_dk),
                              TRUE ~ NA))
    )

  q43 <- paste0("Q", 43:46)
  cars_dummy_data[, q43] <- c(NA,
                              NA,
                              sample(c("some text", NA),
                                     500,
                                     replace = TRUE))


  if(type == "test"){
    cars_dummy_data <- cars_dummy_data %>% mutate(across(c(Q1:Q3,
                                                           Q4.1:Q17,
                                                           Q26.1:Q26.10,
                                                           Q27.1:Q27.10,
                                                           Q28:Q37.7,
                                                           Q38.1:Q39.7,
                                                           Q40:Q42), ~ case_when(row_number() %in% c(3:23) ~ tidyr::replace_na(., "test"),
                                                                                 TRUE ~ .)))
  }

  return(cars_dummy_data)
}







cars_dummy_data <- create_dummy_data(type = "test")
cars_dummy_data_clean <- create_dummy_data(type = "clean")

usethis::use_data(cars_dummy_data, cars_dummy_data_clean, internal = TRUE, overwrite = TRUE)


