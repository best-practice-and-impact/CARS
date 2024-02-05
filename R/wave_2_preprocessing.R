#' Rename columns (wave 2)
#'
#' @description add meaningful column names to dataset ingested from smartsurvey API.
#'
#' @param data CARS wave 2 (2020) survey data (data.frame).
#'
#' @return data.frame
#'
#' @export

w2_rename_cols <- function(data) {

  if (class(data) != "data.frame") {
    stop("Unexpected input: data is not a data.frame.")
  }

  data <- data[c(1, 9:length(data))]

  colnames(data) <- c(
    "ID",
    "tracking_link",
    "department",
    "other_department_name",
    "CS_grade",
    "prof_non_CS",
    "prof_GSG",
    "prof_GES",
    "prof_GSR",
    "prof_GORS",
    "prof_sci_eng",
    "prof_DDAT",
    "prof_GAD",
    "prof_finance",
    "prof_DS_GSG_GORS",
    "prof_DS_other",
    "prof_CS_none",
    "prof_CS_other",
    "region",
    "work_on_official_stat",
    "highest_qualification",
    "degree_maths",
    "degree_stats",
    "degree_systems",
    "degree_comp_sci",
    "degree_econ",
    "degree_psych",
    "degree_geography",
    "degree_other_social_science",
    "degree_life_sci",
    "degree_physical_sci",
    "degree_engineering",
    "degree_business",
    "degree_med_health",
    "degree_law",
    "degree_history",
    "degree_lang_lit",
    "degree_other",
    "code_freq",
    "ops_cleaning",
    "ops_analysis",
    "ops_vis",
    "ops_QA",
    "ops_transfer_migration",
    "ops_other",
    "ops_other_name",
    "knowledge_R",
    "access_R",
    "knowledge_SQL",
    "access_SQL",
    "knowledge_SAS",
    "access_SAS",
    "knowledge_VBA",
    "access_VBA",
    "knowledge_python",
    "access_python",
    "knowledge_SPSS",
    "access_SPSS",
    "knowledge_stata",
    "access_stata",
    "knowledge_JS",
    "access_JS",
    "knowledge_java",
    "access_java",
    "knowledge_C",
    "access_C",
    "knowledge_access_other",
    "experience_outside_role",
    "coding_ability_change",
    "prev_coding_experience",
    "first_learned",
    "heard_of_RAP",
    "RAP_champ",
    "know_RAP_champ",
    "RAP_components",
    "RAP_confident",
    "RAP_important",
    "RAP_supported",
    "RAP_resources",
    "RAP_implementing",
    "RAP_comments",
    "prac_use_open_source",
    "prac_dir_structure",
    "prac_style",
    "prac_version_control",
    "prac_review",
    "prac_functions",
    "prac_package",
    "prac_unit_test",
    "prac_automated_QA",
    "prac_open_source_own",
    "doc_AQA_logs",
    "doc_registers",
    "doc_functions",
    "doc_comments",
    "doc_flow_charts",
    "doc_readme",
    "doc_desk_notes",
    "doc_other",
    "CI",
    "dep_management",
    "reproducible_workflow",
    "code_prac_misc",
    "platform_github",
    "platform_gitlab",
    "platform_bitbucket",
    "platform_aws_codecommit",
    "platform_gcp_cloud_source_repo",
    "platform_other",
    "further_support_comments",
    "further_data_collection",
    "survey_comments"
  )

  return(data)

}

#'@title Enforce streaming rules
#'
#'@description Input the data as a data frame and apply each of the streaming rules in turn.
#'
#'@param data the data which is the output from carsurvey2::tidy_ingest(data)
#'
#'@return the exported data after streaming rules as a dataframe
#'
#'@export

w2_enforce_streaming <- function(data){
  if (class(data) != "data.frame") {
    stop("Unexpected input - data is not a data.frame")
  }

  #If no degree then don't ask page 3
  degree_data <- dplyr::select(data,"degree_maths":"degree_other")
  degree_data[!is.na(data$highest_qualification) & !(data$highest_qualification %in% c("Bachelor's degree (or equivalent)","Master's degree (or equivalent)","Doctoral degree (or equivalent)")),] <- NA
  data[colnames(degree_data)] <- degree_data

  #If code frequency is "Never" then make P12 NA
  freq_data <- dplyr::select(data,"prac_use_open_source":"prac_open_source_own")
  freq_data[!is.na(data$Q8) & data$code_freq == "Never",] <- NA
  data[colnames(freq_data)] <- freq_data

  # if no coding experience outside role"
  exp_data <- dplyr::select(data,"coding_ability_change":"first_learned")
  exp_data[!is.na(data$experience_outside_role) & data$experience_outside_role == "No",] <- NA
  data[colnames(exp_data)] <- exp_data

  # if no prev experience
  data$coding_ability_change <- ifelse(!is.na(data$prev_coding_experience) & data$prev_coding_experience =="Yes", data$coding_ability_change, NA)

  #If if haven't heard of RAP, skip RAP questions
  rap_data <- dplyr::select(data,"RAP_champ":"RAP_comments")
  rap_data[!is.na(data$heard_of_RAP) & data$heard_of_RAP == "No",] <- NA
  data[colnames(rap_data)] <- rap_data

  #If RAP champ, skip RAP champ questions
  data$know_RAP_champ <- ifelse(!is.na(data$RAP_champ) & data$RAP_champ =="Yes" , NA, data$know_RAP_champ)

  #If non git user: skip git platform questions
  git_data <- dplyr::select(data,"platform_aws_codecommit":"platform_other")
  git_data[!is.na(data$prac_version_control) & data$prac_version_control %in% c("Never","I don't understand this question"),] <- NA
  data[colnames(git_data)] <- git_data

  return(data)
}
