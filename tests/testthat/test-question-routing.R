test_that("check_skip_logic returns failing rows from one skipped column", {
  dummy_data <- data.frame(cond_col = c(T, T, F, F), skipped_col = c("test", NA, "test", NA))

  got <- check_skip_logic(dummy_data, dummy_data$cond, "skipped_col")

  expected <- 3

  expect_equal(expected, got)
})

test_that("check_skip_logic returns failing rows from multiple skipped columns", {
  dummy_data <- data.frame(cond_col = c(T, T, T, F, F, F),
                           skipped_col1 = c(NA, NA, NA, "test", NA, NA),
                           skipped_col2 = c(NA, NA, NA, NA, "test", NA),
                           skipped_col3 = c(NA, NA, NA, NA, NA, "test"))

  got <- check_skip_logic(dummy_data, dummy_data$cond, c("skipped_col1", "skipped_col2", "skipped_col3"))

  expected <- c(4, 5, 6)

  expect_equal(expected, got)
})

################################################################################

test_that("enforce_skip_logic replaces failing rows from one skipped column", {
  dummy_data <- data.frame(cond_col = c(T, T, F, F),
                           skipped_col = c("test", NA, "test", NA))

  got <- enforce_skip_logic(dummy_data, dummy_data$cond, "skipped_col")

  expected <- data.frame(cond_col = c(T, T, F, F),
                         skipped_col = c("test", NA, NA, NA))

  expect_equal(expected, got)
})

test_that("enforce_skip_logic replaces failing rows from multiple skipped columns", {
  dummy_data <- data.frame(cond_col = c(T, T, T, F, F, F),
                           skipped_col1 = c(NA, "test", "test", NA, "test", "test"),
                           skipped_col2 = c(NA, NA, "test", NA, NA, "test"),
                           skipped_col3 = c(NA, NA, NA, NA, NA, NA))

  got <- enforce_skip_logic(dummy_data, dummy_data$cond, c("skipped_col1", "skipped_col2", "skipped_col3"))

  expected <- data.frame(cond_col = c(T, T, T, F, F, F),
                         skipped_col1 = c(NA, "test", "test", NA, NA, NA),
                         skipped_col2 = c(NA, NA, "test", NA, NA, NA),
                         skipped_col3 = c(NA, NA, NA, NA, NA, NA))

  expect_equal(expected, got)
})


# ################################################################################
#
# expected <- data.frame(cs_grade = c("Civil service, including devolved administations", "Other"),
#                          department = c("test1", NA),
#                          other_department_name = c("test1", NA),
#                          prof_DS = c("test1", NA),
#                          prof_DDAT = c("test1", NA),
#                          prof_GAD = c("test1", NA),
#                          prof_GES = c("test1", NA),
#                          prof_geog = c("test1", NA),
#                          prof_GORS = c("test1", NA),
#                          prof_GSR = c("test1", NA),
#                          prof_GSG = c("test1", NA),
#                          prof_CS_none = c("test1", NA),
#                          prof_CS_other = c("test1", NA),
#                          ONS_directorate = c("test1", NA))
#
#
# skipped_cols = colnames(expected)[2:length(colnames(expected))]
#
# for(col in skipped_cols) {
#
#   dummy_data <- data.frame(cs_grade = c("Civil service, including devolved administations", "Other"),
#                            department = c("test1", NA),
#                            other_department_name = c("test1", NA),
#                            prof_DS = c("test1", NA),
#                            prof_DDAT = c("test1", NA),
#                            prof_GAD = c("test1", NA),
#                            prof_GES = c("test1", NA),
#                            prof_geog = c("test1", NA),
#                            prof_GORS = c("test1", NA),
#                            prof_GSR = c("test1", NA),
#                            prof_GSG = c("test1", NA),
#                            prof_CS_none = c("test1", NA),
#                            prof_CS_other = c("test1", NA),
#                            ONS_directorate = c("test1", NA))
#
#   dummy_data[2, col] <- "test2"
#
#   condition <- dummy_data$cs_grade == "Civil service, including devolved administations"
#
#   test_that(sprintf("enforce_skip_logic replaces %s. with NAs where cs_grade is 'Civil service, including devolved administations'", col), {
#
#     got <- enforce_skip_logic(data = dummy_data,
#                               condition = condition,
#                               skipped_cols = skipped_cols)
#
#     expect_equal(got, expected)
#
#   })
#
# }
#
# ################################################################################
#
# test_that("enforce_skip_logic replaces ONS_directorate with NAs where department is 'Office for National Statistics'", {
#
#   dummy_data <- data.frame(department = c("Office for National Statistics", "Other"),
#                            ONS_directorate = c("test1", "test2"))
#
#   condition <- dummy_data$department == "Office for National Statistics"
#
#   got <- enforce_skip_logic(data = dummy_data,
#                             condition = condition,
#                             skipped_cols = "ONS_directorate")
#
#   expected <- data.frame(department = c("Office for National Statistics", "Other"),
#                          ONS_directorate = c("test1", NA))
#
#   expect_equal(got, expected)
#
# })
#
# ################################################################################
#
# expected <- data.frame(highest_qualification = c("Any other qualification", "Other"),
#                        qual_1_subject = c(NA, "test2"),
#                        qual_1_level = c(NA, "test2"),
#                        qual_1_learn_code = c(NA, "test2"),
#                        qual_2_subject = c(NA, "test2"),
#                        qual_2_level = c(NA, "test2"),
#                        qual_2_learn_code = c(NA, "test2"),
#                        qual_3_subject = c(NA, "test2"),
#                        qual_3_level = c(NA, "test2"),
#                        qual_3_learn_code = c(NA, "test2"))
#
# skipped_cols = colnames(expected)[2:length(colnames(expected))]
#
# for(col in skipped_cols) {
#
#   dummy_data <- data.frame(highest_qualification = c("Any other qualification", "Other"),
#                            qual_1_subject = c(NA, "test2"),
#                            qual_1_level = c(NA, "test2"),
#                            qual_1_learn_code = c(NA, "test2"),
#                            qual_2_subject = c(NA, "test2"),
#                            qual_2_level = c(NA, "test2"),
#                            qual_2_learn_code = c(NA, "test2"),
#                            qual_3_subject = c(NA, "test2"),
#                            qual_3_level = c(NA, "test2"),
#                            qual_3_learn_code = c(NA, "test2"))
#
#   dummy_data[1, col] <- "test1"
#
#   condition <- dummy_data$highest_qualification != "Any other qualification"
#
#   test_that(sprintf("enforce_skip_logic replaces %s. with NAs where highest_qualifcation is 'Any other qualification'", col), {
#
#     got <- enforce_skip_logic(data = dummy_data,
#                               condition = condition,
#                               skipped_cols = skipped_cols)
#
#     expect_equal(got, expected)
#
#   })
#
# }
#
# ################################################################################
#
# expected <- data.frame(code_freq = c("Never", "Other"),
#                        prac_use_open_source = c(NA, "test2"),
#                        prac_open_source_own = c(NA, "test2"),
#                        prac_version_control = c(NA, "test2"),
#                        prac_review = c(NA, "test2"),
#                        prac_functions = c(NA, "test2"),
#                        prac_unit_test = c(NA, "test2"),
#                        prac_package = c(NA, "test2"),
#                        prac_dir_structure = c(NA, "test2"),
#                        prac_style = c(NA, "test2"),
#                        prac_automated_QA = c(NA, "test2"),
#                        prac_AQUA_book = c(NA, "test2"),
#                        doc_comments = c(NA, "test2"),
#                        doc_functions = c(NA, "test2"),
#                        doc_readme = c(NA, "test2"),
#                        doc_desk_notes = c(NA, "test2"),
#                        doc_registers = c(NA, "test2"),
#                        doc_AQA_logs = c(NA, "test2"),
#                        doc_flow_charts = c(NA, "test2"),
#                        doc_other = c(NA, "test2"),
#                        CI = c(NA, "test2"),
#                        dep_management = c(NA, "test2"),
#                        reproducible_workflow = c(NA, "test2"),
#                        misc_coding = c(NA, "test2"))
#
# skipped_cols = colnames(expected)[2:length(colnames(expected))]
#
# for(col in skipped_cols) {
#
#   dummy_data <- data.frame(code_freq = c("Never", "Other"),
#                            prac_use_open_source = c(NA, "test2"),
#                            prac_open_source_own = c(NA, "test2"),
#                            prac_version_control = c(NA, "test2"),
#                            prac_review = c(NA, "test2"),
#                            prac_functions = c(NA, "test2"),
#                            prac_unit_test = c(NA, "test2"),
#                            prac_package = c(NA, "test2"),
#                            prac_dir_structure = c(NA, "test2"),
#                            prac_style = c(NA, "test2"),
#                            prac_automated_QA = c(NA, "test2"),
#                            prac_AQUA_book = c(NA, "test2"),
#                            doc_comments = c(NA, "test2"),
#                            doc_functions = c(NA, "test2"),
#                            doc_readme = c(NA, "test2"),
#                            doc_desk_notes = c(NA, "test2"),
#                            doc_registers = c(NA, "test2"),
#                            doc_AQA_logs = c(NA, "test2"),
#                            doc_flow_charts = c(NA, "test2"),
#                            doc_other = c(NA, "test2"),
#                            CI = c(NA, "test2"),
#                            dep_management = c(NA, "test2"),
#                            reproducible_workflow = c(NA, "test2"),
#                            misc_coding = c(NA, "test2"))
#
#   dummy_data[1, col] <- "test1"
#
#   condition <- dummy_data$code_freq != "Never"
#
#   test_that(sprintf("enforce_skip_logic replaces %s. with NAs where code_freq is 'Never'", col), {
#
#     got <- enforce_skip_logic(data = dummy_data,
#                               condition = condition,
#                               skipped_cols = skipped_cols)
#
#     expect_equal(got, expected)
#
#   })
#
# }
#
# ################################################################################
#
# expected <- data.frame(other_coding_experience = c("No", "Yes"),
#                        coding_ability_change = c(NA, "test2"),
#                        prev_coding_experience = c(NA, "test2"),
#                        first_learned = c(NA, "test2"))
#
# skipped_cols = colnames(expected)[2:length(colnames(expected))]
#
# for(col in skipped_cols) {
#
#   dummy_data <- data.frame(other_coding_experience = c("No", "Yes"),
#                            coding_ability_change = c(NA, "test2"),
#                            prev_coding_experience = c(NA, "test2"),
#                            first_learned = c(NA, "test2"))
#
#   dummy_data[1, col] <- "test1"
#
#   condition <- dummy_data$other_coding_experience != "No"
#
#   test_that(sprintf("enforce_skip_logic replaces %s. with NAs where other_coding_experience is 'No'", col), {
#
#     got <- enforce_skip_logic(data = dummy_data,
#                               condition = condition,
#                               skipped_cols = skipped_cols)
#
#     expect_equal(got, expected)
#
#   })
#
# }
#
# ################################################################################
#
# test_that("enforce_skip_logic replaces first_learned with NAs where prev_coding_experience is 'No'", {
#
#   dummy_data <- data.frame(prev_coding_experience = c("No", "Yes"),
#                            first_learned = c("test1", "test2"))
#
#   condition <- dummy_data$prev_coding_experience != "No"
#
#   got <- enforce_skip_logic(data = dummy_data,
#                             condition = condition,
#                             skipped_cols = "first_learned")
#
#   expected <- data.frame(prev_coding_experience = c("No", "Yes"),
#                          first_learned = c(NA, "test2"))
#
#   expect_equal(got, expected)
#
# })
#
# ################################################################################
#
# expected <- data.frame(heard_of_RAP = c("No", "yes"),
#                        know_RAP_champ = c(NA, "test2"),
#                        strategy_knowledge = c(NA, "test2"),
#                        RAP_confident = c(NA, "test2"),
#                        RAP_supported = c(NA, "test2"),
#                        RAP_resources = c(NA, "test2"),
#                        RAP_components = c(NA, "test2"),
#                        RAP_important = c(NA, "test2"),
#                        RAP_implementing = c(NA, "test2"),
#                        RAP_planning = c(NA, "test2"),
#                        RAP_comments = c(NA, "test2"))
#
# skipped_cols = colnames(expected)[2:length(colnames(expected))]
#
# for(col in skipped_cols) {
#
#   dummy_data <- data.frame(heard_of_RAP = c("No", "yes"),
#                            know_RAP_champ = c(NA, "test2"),
#                            strategy_knowledge = c(NA, "test2"),
#                            RAP_confident = c(NA, "test2"),
#                            RAP_supported = c(NA, "test2"),
#                            RAP_resources = c(NA, "test2"),
#                            RAP_components = c(NA, "test2"),
#                            RAP_important = c(NA, "test2"),
#                            RAP_implementing = c(NA, "test2"),
#                            RAP_planning = c(NA, "test2"),
#                            RAP_comments = c(NA, "test2"))
#
#   dummy_data[1, col] <- "test1"
#
#   condition <- dummy_data$heard_of_RAP != "No"
#
#   test_that(sprintf("enforce_skip_logic replaces %s. with NAs where heard_of_RAP is 'No'", col), {
#
#     got <- enforce_skip_logic(data = dummy_data,
#                               condition = condition,
#                               skipped_cols = skipped_cols)
#
#     expect_equal(got, expected)
#
#   })
#
# }
#
################################################################################

questions = c("workplace",
              "department",
              "highest_qualification",
              "code_freq",
              "other_coding_experience",
              "prev_coding_experience",
              "heard_of_RAP")

conditions = c("Civil service, including devolved administations",
               "Office for National Statistics",
               "Any other qualification",
               "Never",
               "No",
               "No",
               "No")

data_template <- data.frame(workplace = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         cs_grade = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         department = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         other_department_name = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prof_DS = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prof_DDAT = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prof_GAD = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prof_GES = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prof_geog = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prof_GORS = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prof_GSR = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prof_GSG = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prof_CS_none = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prof_CS_other = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         ONS_directorate = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         highest_qualification = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         qual_1_subject = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         qual_1_level = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         qual_1_learn_code = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         qual_2_subject = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         qual_2_level = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         qual_2_learn_code = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         qual_3_subject = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         qual_3_level = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         qual_3_learn_code = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         code_freq = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         other_coding_experience = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         coding_ability_change = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prev_coding_experience = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         first_learned = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         heard_of_RAP = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         know_RAP_champ = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         strategy_knowledge = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         RAP_confident = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         RAP_supported = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         RAP_resources = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         RAP_components = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         RAP_important = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         RAP_implementing = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         RAP_planning = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         RAP_comments = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_use_open_source = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_open_source_own = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_version_control = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_review = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_functions = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_unit_test = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_package = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_dir_structure = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_style = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_automated_QA = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         prac_AQUA_book = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         doc_comments = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         doc_functions = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         doc_readme = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         doc_desk_notes = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         doc_registers = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         doc_AQA_logs = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         doc_flow_charts = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         doc_other = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         CI = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         dep_management = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         reproducible_workflow = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"),
                         misc_coding = c("test1", "test2", "test3", "test4", "test5", "test6", "test7"))

skipped_cols <- list(colnames(data_template)[which(colnames(data_template) == "cs_grade"):which(colnames(data_template) == "ONS_directorate")],
                     "ONS_directorate",
                     colnames(data_template)[which(colnames(data_template) == "qual_1_subject"):which(colnames(data_template) == "qual_3_learn_code")],
                     colnames(data_template)[which(colnames(data_template) == "prac_use_open_source"):which(colnames(data_template) == "misc_coding")],
                     colnames(data_template)[which(colnames(data_template) == "coding_ability_change"):which(colnames(data_template) == "first_learned")],
                     "first_learned",
                     colnames(data_template)[which(colnames(data_template) == "know_RAP_champ"):which(colnames(data_template) == "RAP_comments")])

dummy_data <- data_template
expected <- data_template

for(i in 1:length(questions)) {

  dummy_data[i, questions[i]] <- conditions[i]
  expected[i, questions[i]] <- conditions[i]

  if (i==1) {
    expected[-i, skipped_cols[[1]]] <- NA
    expected[i, skipped_cols[[2]]] <- NA
  } else if (i==2) {
    expected[i, questions[i]] <- NA
  } else {
    expected[i, skipped_cols[[i]]] <- NA
  }

  test_that(sprintf("apply_skip_logic replaces relevant skipped question enteries with NAs where %s is '%s.'", questions[i], conditions[i]), {

    got <- apply_skip_logic(data = dummy_data)

    expect_equal(got, expected)

  })

}
