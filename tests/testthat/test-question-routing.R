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

data_template <- data.frame(workplace = rep("test", 7),
                         cs_grade = rep("test", 7),
                         department = rep("test", 7),
                         other_department_name = rep("test", 7),
                         prof_DS = rep("test", 7),
                         prof_DDAT = rep("test", 7),
                         prof_GAD = rep("test", 7),
                         prof_GES = rep("test", 7),
                         prof_geog = rep("test", 7),
                         prof_GORS = rep("test", 7),
                         prof_GSR = rep("test", 7),
                         prof_GSG = rep("test", 7),
                         prof_CS_none = rep("test", 7),
                         prof_CS_other = rep("test", 7),
                         ONS_directorate = rep("test", 7),
                         highest_qualification = rep("test", 7),
                         qual_1_subject = rep("test", 7),
                         qual_1_level = rep("test", 7),
                         qual_1_learn_code = rep("test", 7),
                         qual_2_subject = rep("test", 7),
                         qual_2_level = rep("test", 7),
                         qual_2_learn_code = rep("test", 7),
                         qual_3_subject = rep("test", 7),
                         qual_3_level = rep("test", 7),
                         qual_3_learn_code = rep("test", 7),
                         code_freq = rep("test", 7),
                         other_coding_experience = rep("test", 7),
                         coding_ability_change = rep("test", 7),
                         prev_coding_experience = rep("test", 7),
                         first_learned = rep("test", 7),
                         heard_of_RAP = rep("test", 7),
                         know_RAP_champ = rep("test", 7),
                         strategy_knowledge = rep("test", 7),
                         RAP_confident = rep("test", 7),
                         RAP_supported = rep("test", 7),
                         RAP_resources = rep("test", 7),
                         RAP_components = rep("test", 7),
                         RAP_important = rep("test", 7),
                         RAP_implementing = rep("test", 7),
                         RAP_planning = rep("test", 7),
                         RAP_comments = rep("test", 7),
                         prac_use_open_source = rep("test", 7),
                         prac_open_source_own = rep("test", 7),
                         prac_version_control = rep("test", 7),
                         prac_review = rep("test", 7),
                         prac_functions = rep("test", 7),
                         prac_unit_test = rep("test", 7),
                         prac_package = rep("test", 7),
                         prac_dir_structure = rep("test", 7),
                         prac_style = rep("test", 7),
                         prac_automated_QA = rep("test", 7),
                         prac_AQUA_book = rep("test", 7),
                         doc_comments = rep("test", 7),
                         doc_functions = rep("test", 7),
                         doc_readme = rep("test", 7),
                         doc_desk_notes = rep("test", 7),
                         doc_registers = rep("test", 7),
                         doc_AQA_logs = rep("test", 7),
                         doc_flow_charts = rep("test", 7),
                         doc_other = rep("test", 7),
                         CI = rep("test", 7),
                         dep_management = rep("test", 7),
                         reproducible_workflow = rep("test", 7),
                         misc_coding = rep("test", 7))

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
