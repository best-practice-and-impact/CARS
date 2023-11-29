
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

