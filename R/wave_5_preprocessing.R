
#' @title Rename columns
#'
#' @description Renames columns and removes unnecessary columns
#'
#' @param data tidy CARS dataset
#'
#' @return data.frame
#'
#' @export

w5_rename_cols <- function(data) {
  if (ncol(data) != 112) {
    stop("Unexpected input: incorrect number of columns. Please use the 2023 CARS dataset.")
  }

  colnames(data)[c(1, 7:ncol(data))] <- c(
    "ID",
    "started",
    "ended",
    "tracking_link",
    "workplace",
    "CS_grade",
    "department",
    "other_department_name",
    "prof_DE",
    "prof_DS",
    "prof_DDAT",
    "prof_GAD",
    "prof_GES",
    "prof_geog",
    "prof_GORS",
    "prof_GSR",
    "prof_GSG",
    "prof_CS_none",
    "prof_CS_other",
    "ONS_directorate",
    "pay_band",
    "NHS_band",
    "NJC_grade",
    "primary_work_country",
    "England_NHS_organisation",
    "Scotland_NHS_organisation",
    "Wales_NHS_organisation",
    "Northern_Ireland_NHS_organisation",
    "highest_qualification",
    "qual_1_subject",
    "qual_1_level",
    "qual_1_learn_code",
    "qual_2_subject",
    "qual_2_level",
    "qual_2_learn_code",
    "qual_3_subject",
    "qual_3_level",
    "qual_3_learn_code",
    "code_freq",
    "management",
    "access_matlab",
    "access_python",
    "access_R",
    "access_SAS",
    "access_SPSS",
    "access_SQL",
    "access_stata",
    "access_VBA",
    "access_open_source_other",
    "access_licensed_other",
    "access_other_specified",
    "knowledge_matlab",
    "knowledge_python",
    "knowledge_R",
    "knowledge_SAS",
    "knowledge_SPSS",
    "knowledge_SQL",
    "knowledge_stata",
    "knowledge_VBA",
    "knowledge_licensed_other",
    "knowledge_open_source_other",
    "knowledge_other_specified",
    "knowledge_git",
    "access_git",
    "other_coding_experience",
    "first_learned",
    "coding_ability_change",
    "heard_of_RAP",
    "have_RAP_champ",
    "know_RAP_champ",
    "strategy_knowledge",
    "RAP_confident",
    "RAP_supported",
    "RAP_resources",
    "RAP_components",
    "RAP_important",
    "RAP_implementing",
    "RAP_planning",
    "RAP_comments",
    "prac_use_open_source",
    "prac_open_source_own",
    "prac_version_control",
    "prac_review",
    "prac_functions",
    "prac_unit_test",
    "prac_other_automated",
    "prac_package",
    "prac_dir_structure",
    "prac_style",
    "prac_automated_QA",
    "prac_development_QA",
    "prac_proportionate_QA",
    "doc_comments",
    "doc_functions",
    "doc_readme",
    "doc_desk_notes",
    "doc_registers",
    "doc_AQA_logs",
    "doc_flow_charts",
    "doc_other",
    "CI",
    "dep_management",
    "reproducible_workflow",
    "misc_coding",
    "misc_support",
    "misc_additional_data",
    "misc_other"
  )

  data <- data[!colnames(data) %in% c("UserNo", "Name", "Email", "IP.Address", "Unique.ID")]

  return(data)
}

#' @title Clean data
#'
#' @description Recategorise department, workplace and first_learned data
#'
#' @param data cleaned CARS dataset
#'
#' @return CARS dataset
#' @export

w5_clean_data <- function(data){

  data %>%
    w5_clean_departments() %>%
    w5_clean_workplace() %>%
    w5_clean_first_learned()

}


#' @title Clean department data
#'
#' @description add NHS to department list and merge departments where needed.
#'
#' @param data cleaned CARS dataset
#'
#' @return CARS dataset
#' @export

w5_clean_departments <- function(data) {

  data$department[data$department == "Foreign, Commonwealth & Development Office (excl. agencies)"] <- "Foreign, Commonwealth and Development Office (excl. agencies)"

  data$department[data$workplace == "NHS"] <- "NHS"

  data$department[data$other_department_name == "Office for National Statistics"] <- "Office for National Statistics"

  data$department[data$other_department_name == "Data Science Campus"] <- "Office for National Statistics"

  data$department[data$other_department_name == "Welsh Revenue Authority"] <- "Welsh Government"

  data$department[data$other_department_name == "Equality Hub, Cabinet Office"] <- "Cabinet Office (excl. agencies)"

  data$department[data$other_department_name == "Natural England"] <- "Natural England"

  data$department[data$other_department_name == "Department for Communities"] <- "Northern Ireland Executive"

  data$department[data$other_department_name == "Department of Education Northern Ireland"] <- "Northern Ireland Executive"

  defra_orgs <- c(
    "Department for Environment, Food and Rural Affairs (excl. agencies)",
    "Forestry Commission",
    "Forest Research",
    "Forestry England",
    "Animal and Plant Health Agency",
    "Centre for Environment, Fisheries and Aquaculture Science",
    "Rural Payments Agency",
    "Environment Agency",
    "Marine Management Organisation",
    "Natural England"
  )

  data$defra <- data$department %in% defra_orgs

  return(data)

}

#' @title Clean workplace data
#'
#' @description reclassify 'other' text responses into CS/NHS
#'
#' @param data cleaned CARS dataset
#'
#' @return CARS dataset
#' @export

w5_clean_workplace <- function(data) {

  data$workplace[data$workplace == "MOD"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "HMRC"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "The Pensions Regulator"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "Scottish Funding Council"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "Office for Students"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "Office for students"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "OfS"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "Dstl"] <- "Civil service, including devolved administrations"

  return(data)

}

#' @title Clean first learned data
#'
#' @description reclassify 'other' free text responses into self-taught based on common terms used
#'
#' @param data cleaned CARS dataset
#'
#' @return CARS dataset
#' @export

w5_clean_first_learned <- function(data) {

  matches <- c("self",
               "hobby",
               "personal",
               "independ",
               "home",
               "for fun",
               "free time",
               "spare time",
               "childhood")

  data$first_learned[stringr::str_detect(tolower(data$first_learned), stringr::str_c(matches, collapse = "|"))] <- "Self-taught"

  return(data)

}

#' @title Apply skip logic
#'
#' @description Iteratively applies enforce_skip_logic to the necessary fields in the data.
#'
#' @param data data.frame
#'
#' @return cleaned data.frame
#'
#' @export

w5_apply_skip_logic <- function(data) {

  conditions <- list(data$workplace == "NHS or local healthcare service",
                     !data$workplace %in% c("Civil service, including devolved administrations", "NHS or local healthcare service") & !is.na(data$workplace) & data$workplace != "test",
                     data$department != "Office for National Statistics" & !is.na(data$department) & data$department != "test",
                     !is.na(data$ONS_directorate) & data$ONS_directorate != "test",
                     data$pay_band == "Local Authority or NJC",
                     data$pay_band == "Other / Not sure",
                     !is.na(data$NHS_band) & data$NHS_band != "test",
                     data$primary_work_country == "Scotland",
                     data$primary_work_country == "Wales",
                     data$primary_work_country == "Northern Ireland",
                     !is.na(data$England_NHS_organisation) & data$England_NHS_organisation != "test",
                     !is.na(data$Scotland_NHS_organisation) & data$Scotland_NHS_organisation != "test",
                     !is.na(data$Wales_NHS_organisation) & data$Wales_NHS_organisation != "test",
                     data$highest_qualification == "Any other qualification",
                     data$code_freq == "Never",
                     data$other_coding_experience == "No",
                     data$heard_of_RAP == "No",
                     data$have_RAP_champ != "Yes" & !is.na(data$have_RAP_champ) & data$have_RAP_champ != "test")

  skipped_cols <- list(colnames(dplyr::select(data, "CS_grade":"ONS_directorate")),
                       colnames(dplyr::select(data, "CS_grade":"Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "ONS_directorate":"Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "pay_band":"Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "NHS_band")),
                       colnames(dplyr::select(data, "NHS_band":"NJC_grade")),
                       colnames(dplyr::select(data, "NJC_grade")),
                       colnames(dplyr::select(data, "England_NHS_organisation")),
                       colnames(dplyr::select(data, "England_NHS_organisation":"Scotland_NHS_organisation")),
                       colnames(dplyr::select(data, "England_NHS_organisation":"Wales_NHS_organisation")),
                       colnames(dplyr::select(data, "Scotland_NHS_organisation":"Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "Wales_NHS_organisation":"Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "qual_1_subject":"qual_3_learn_code")),
                       colnames(dplyr::select(data, "other_coding_experience":"reproducible_workflow")),
                       colnames(dplyr::select(data, "first_learned":"coding_ability_change")),
                       colnames(dplyr::select(data, "have_RAP_champ":"RAP_comments")),
                       colnames(dplyr::select(data, "know_RAP_champ")))

  for(i in 1:length(conditions)){
    data <- w5_enforce_skip_logic(data, conditions[[i]], skipped_cols[[i]])
  }

  return(data)

}

#' @title Check skip logic
#'
#' @description Checks whether the skip logic was followed correctly. Backtracking while filling the survey can result in inconsistent response sets.
#' This check returns row numbers where questions which should have been skipped contain anything other than NA.
#'
#' @param data data.frame
#' @param condition logical vector. Example: data$row == "skip response"
#' @param skipped_cols character. questions that should have been skipped if condition != TRUE
#'
#' @return list of rows failing the check
#'
#' @export

w5_check_skip_logic <- function(data, condition, skipped_cols) {

  condition_met <- condition & !is.na(data[skipped_cols])

  row_failed <- as.logical(rowSums(condition_met))

  return(
    which(row_failed)
  )

}

#' @title enforce skip logic
#'
#' @description Replaces values in rows with NAs where check_skip_logic has identified backtracking.
#'
#' @param data data.frame
#' @param condition logical vector. Example: data$row == "skip response"
#' @param skipped_cols character. questions that should have been skipped if condition != TRUE
#'
#' @return data.frame with rows failing the check replaced with NAs
#'
#' @export

w5_enforce_skip_logic <- function(data, condition, skipped_cols) {

  row_index <- w5_check_skip_logic(data, condition, skipped_cols)

  data[row_index, skipped_cols] <- NA

  return(data)

}

#' @title Derive variables
#'
#' @description API function for deriving additional variables.
#'
#' @param data tidied and relabelled CARS wave 3 dataset.
#'
#' @return data (data.frame).
#'
#' @export

w5_derive_vars <- function(data) {
  data <- data %>%
    w5_derive_language_status() %>%
    w5_derive_rap_score() %>%
    w5_derive_rap_champ_status()

  return(data)
}



#' @title Derive language status
#'
#' @description Derive the status of each programmming language as "access" (access only), "knowledge" (knowledge only), "both" or "neither".
#'
#' @param data tidied CARS wave 3 data (data.frame).
#'
#' @return data.frame

w5_derive_language_status <- function(data) {

  lang_list <- colnames(data)[grepl("access_", colnames(data))]

  lang_list <- lang_list[!grepl("other", lang_list)]

  lang_list <- gsub("access_", "", lang_list)

  new_vars <- sapply(lang_list, function(lang) {
    access_col <- data[[paste0("access_", lang)]]

    knowledge_col <- data[[paste0("knowledge_", lang)]]

    dplyr::case_when(access_col == "Yes" & knowledge_col == "Yes" ~ "Both",
                     access_col == "Yes" & knowledge_col != "Yes" ~ "Access Only",
                     access_col != "Yes" & knowledge_col == "Yes" ~ "Knowledge Only",
                     access_col != "Yes" & knowledge_col != "Yes" ~ "Neither")
  })

  colnames(new_vars) <- paste0("status_", lang_list)

  return(data.frame(data, new_vars))
}


#'@title Derive RAP scores
#'
#'@description Derive basic and advanced RAP score columns from existing variables and add to the dataframe.
#'
#'@param data a date frame containing cleaned CARS wave 3 data
#'
#'@return dataframe containing the additional RAP score columns
#'

w5_derive_rap_score <- function(data){
  data <- w5_derive_basic_rap_scores(data)
  data <- w5_derive_advanced_rap_scores(data)

  return(data)
}


#' @title Derive basic RAP scores
#'
#' @description Derive basic RAP score columns from existing variables and add to the dataframe.
#'
#' @param data a date frame containing cleaned CARS wave 3 data
#'
#' @return dataframe containing the additional basic RAP score columns
#'
#' @importFrom dplyr mutate across case_when rename_with all_of

w5_derive_basic_rap_scores <- function(data) {

  expected_columns <- c("code_freq",
                        "prac_use_open_source",
                        "prac_open_source_own",
                        "prac_version_control",
                        "prac_review",
                        "prac_proportionate_QA",
                        "doc_comments",
                        "doc_readme")

  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  } else if (!all(expected_columns %in% colnames(data))) {
    missing <- paste(expected_columns[!(expected_columns %in% colnames(data))], collapse = "\n")
    stop(
      paste0("Unexpected input - missing column names: ", missing)
    )
  }

  score_col_names <- c("use_open_source_score",
                       "open_code_score",
                       "version_control_score",
                       "peer_review_score",
                       "proportionate_QA_score",
                       "doc_score")

  high_vals <- c("Regularly", "All the time")

  prac_cols <- expected_columns[!(expected_columns %in% c("code_freq", "doc_comments", "doc_readme"))]

  data <- data %>%
    mutate(across(.cols = expected_columns[expected_columns != "code_freq"],
                  ~ case_when(code_freq == "Never" ~ NA_real_,
                              .x %in% high_vals ~ 1,
                              TRUE ~ 0),
                  .names = "{.col}_score")) %>%
    mutate(doc_score = as.integer(doc_comments_score & doc_readme_score)) %>%
    select(-c(doc_comments_score, doc_readme_score)) %>%
    rename_with(~ score_col_names[which(paste0(prac_cols, "_score") == .x)],
                .cols = paste0(prac_cols,
                               "_score")) %>%
    mutate(basic_rap_score = rowSums(across(all_of(score_col_names))))

  return(data)

}


#' @title Derive advanced RAP scores
#'
#' @description Derive advanced RAP score columns from existing variables and add to the dataframe.
#'
#' @param data a date frame containing cleaned CARS wave 3 data
#'
#' @return dataframe containing the additional advanced RAP score columns
#'
#' @importFrom dplyr mutate across case_when rename_with all_of

w5_derive_advanced_rap_scores <- function(data) {

  expected_columns <- c("code_freq",
                        "prac_functions",
                        "prac_unit_test",
                        "doc_functions",
                        "prac_package",
                        "prac_style",
                        "CI",
                        "dep_management")

  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  } else if (!all(expected_columns %in% colnames(data))) {
    missing <- paste(expected_columns[!(expected_columns %in% colnames(data))], collapse = "\n")
    stop(
      paste0("Unexpected input - missing column names: ", missing)
    )
  }

  score_col_names <- c("function_score",
                       "unit_test_score",
                       "function_doc_score",
                       "package_score",
                       "code_style_score",
                       "cont_integration_score",
                       "dep_management_score")

  high_vals <- c("Regularly", "All the time", "Yes")

  data <- data %>%
    mutate(across(.cols = expected_columns[expected_columns != "code_freq"],
                  ~ case_when(code_freq == "Never" ~ NA_real_,
                              .x %in% high_vals ~ 1,
                              TRUE ~ 0),
                  .names = "{.col}_score")) %>%
    rename_with(~ score_col_names[which(paste0(expected_columns[expected_columns != "code_freq"], "_score") == .x)],
                .cols = paste0(expected_columns[expected_columns != "code_freq"],
                               "_score")) %>%
    mutate(advanced_rap_score = rowSums(across(all_of(score_col_names))))

  return(data)

}


#' @title Derive RAP Champion status
#'
#' @description Derive RAP Champion status column from existing variables and add to the dataframe.
#'
#' @param data a date frame containing cleaned CARS wave 5 data
#'
#' @return dataframe containing the additional RAP Champion status columns
#'
#' @importFrom dplyr mutate case_when
w5_derive_rap_champ_status <- function(data){

  data <- data %>%
    mutate(RAP_champ_status = case_when(have_RAP_champ == "Yes" & know_RAP_champ == "Yes, and I am a RAP Champion" ~ "Yes, and I am a RAP Champion",
                                        have_RAP_champ == "Yes" & know_RAP_champ == "Yes" ~ "Yes, and I know who the RAP Champion is",
                                        have_RAP_champ == "Yes" & know_RAP_champ == "No" ~ "Yes, but I don't know who the RAP Champion is",
                                        have_RAP_champ == "No" ~ "No",
                                        have_RAP_champ == "Don't know" ~ "I don't know"))

}



