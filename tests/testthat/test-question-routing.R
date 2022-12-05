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


questions = c("workplace",
              "department",
              "highest_qualification",
              "code_freq",
              "other_coding_experience",
              "prev_coding_experience",
              "heard_of_RAP")

conditions = c("NHS",
               "Companies House",
               "Any other qualification",
               "Never",
               "No",
               "No",
               "No")

data_template <- data.frame(workplace = c("test", "test", "test", "test", "test", "test", "test"),
                         cs_grade = c("test", "test", "test", "test", "test", "test", "test"),
                         department = c("test", "test", "test", "test", "test", "test", "test"),
                         other_department_name = c("test", "test", "test", "test", "test", "test", "test"),
                         prof_DS = c("test", "test", "test", "test", "test", "test", "test"),
                         prof_DDAT = c("test", "test", "test", "test", "test", "test", "test"),
                         prof_GAD = c("test", "test", "test", "test", "test", "test", "test"),
                         prof_GES = c("test", "test", "test", "test", "test", "test", "test"),
                         prof_geog = c("test", "test", "test", "test", "test", "test", "test"),
                         prof_GORS = c("test", "test", "test", "test", "test", "test", "test"),
                         prof_GSR = c("test", "test", "test", "test", "test", "test", "test"),
                         prof_GSG = c("test", "test", "test", "test", "test", "test", "test"),
                         prof_CS_none = c("test", "test", "test", "test", "test", "test", "test"),
                         prof_CS_other = c("test", "test", "test", "test", "test", "test", "test"),
                         ONS_directorate = c("test", "test", "test", "test", "test", "test", "test"),
                         highest_qualification = c("test", "test", "test", "test", "test", "test", "test"),
                         qual_1_subject = c("test", "test", "test", "test", "test", "test", "test"),
                         qual_1_level = c("test", "test", "test", "test", "test", "test", "test"),
                         qual_1_learn_code = c("test", "test", "test", "test", "test", "test", "test"),
                         qual_2_subject = c("test", "test", "test", "test", "test", "test", "test"),
                         qual_2_level = c("test", "test", "test", "test", "test", "test", "test"),
                         qual_2_learn_code = c("test", "test", "test", "test", "test", "test", "test"),
                         qual_3_subject = c("test", "test", "test", "test", "test", "test", "test"),
                         qual_3_level = c("test", "test", "test", "test", "test", "test", "test"),
                         qual_3_learn_code = c("test", "test", "test", "test", "test", "test", "test"),
                         code_freq = c("test", "test", "test", "test", "test", "test", "test"),
                         other_coding_experience = c("test", "test", "test", "test", "test", "test", "test"),
                         coding_ability_change = c("test", "test", "test", "test", "test", "test", "test"),
                         prev_coding_experience = c("test", "test", "test", "test", "test", "test", "test"),
                         first_learned = c("test", "test", "test", "test", "test", "test", "test"),
                         heard_of_RAP = c("test", "test", "test", "test", "test", "test", "test"),
                         know_RAP_champ = c("test", "test", "test", "test", "test", "test", "test"),
                         strategy_knowledge = c("test", "test", "test", "test", "test", "test", "test"),
                         RAP_confident = c("test", "test", "test", "test", "test", "test", "test"),
                         RAP_supported = c("test", "test", "test", "test", "test", "test", "test"),
                         RAP_resources = c("test", "test", "test", "test", "test", "test", "test"),
                         RAP_components = c("test", "test", "test", "test", "test", "test", "test"),
                         RAP_important = c("test", "test", "test", "test", "test", "test", "test"),
                         RAP_implementing = c("test", "test", "test", "test", "test", "test", "test"),
                         RAP_planning = c("test", "test", "test", "test", "test", "test", "test"),
                         RAP_comments = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_use_open_source = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_open_source_own = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_version_control = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_review = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_functions = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_unit_test = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_package = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_dir_structure = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_style = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_automated_QA = c("test", "test", "test", "test", "test", "test", "test"),
                         prac_AQUA_book = c("test", "test", "test", "test", "test", "test", "test"),
                         doc_comments = c("test", "test", "test", "test", "test", "test", "test"),
                         doc_functions = c("test", "test", "test", "test", "test", "test", "test"),
                         doc_readme = c("test", "test", "test", "test", "test", "test", "test"),
                         doc_desk_notes = c("test", "test", "test", "test", "test", "test", "test"),
                         doc_registers = c("test", "test", "test", "test", "test", "test", "test"),
                         doc_AQA_logs = c("test", "test", "test", "test", "test", "test", "test"),
                         doc_flow_charts = c("test", "test", "test", "test", "test", "test", "test"),
                         doc_other = c("test", "test", "test", "test", "test", "test", "test"),
                         CI = c("test", "test", "test", "test", "test", "test", "test"),
                         dep_management = c("test", "test", "test", "test", "test", "test", "test"),
                         reproducible_workflow = c("test", "test", "test", "test", "test", "test", "test"),
                         misc_coding = c("test", "test", "test", "test", "test", "test", "test"))

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

  expected[i, skipped_cols[[i]]] <- NA

  test_that(sprintf("apply_skip_logic replaces relevant skipped question enteries with NAs where %s is '%s.'", questions[i], conditions[i]), {

    got <- apply_skip_logic(data = dummy_data)

    expect_equal(got, expected)

  })

}
