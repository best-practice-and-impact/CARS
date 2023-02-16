#' Rename columns (wave 3)
#'
#' @description add meaningful column names to dataset ingested from smartsurvey API.
#'
#' @param data CARS wave 3 (2021) survey data (data.frame).
#'
#' @return data.frame
#'
#' @export

w3_rename_cols <- function(data) {

  if (class(data) != "data.frame") {
    stop("Unexpected input: data is not a data.frame.")
  }

  data <- data[c(1, 9:length(data))]

  colnames(data) <- c(
    "ID",
    "tracking_link",
    "CS_grade",
    "department",
    "other_department_name",
    "prof_non_CS",
    "prof_DS_GSG_GORS",
    "prof_DS_other",
    "prof_DDAT",
    "prof_GAD",
    "prof_GES",
    "prof_finance",
    "prof_geog",
    "prof_GORS",
    "prof_GSR",
    "prof_GSG",
    "prof_CS_none",
    "prof_CS_other",
    "region",
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
    "degree_earth_sci",
    "degree_engineering",
    "degree_business",
    "degree_med_health",
    "degree_law",
    "degree_history",
    "degree_lang_lit",
    "degree_other",
    "code_freq",
    "code_manage",
    "ops_cleaning",
    "ops_analysis",
    "ops_linking",
    "ops_transfer_migration",
    "ops_vis",
    "ops_machine_learning",
    "ops_modelling",
    "ops_QA",
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
    "knowledge_matlab",
    "access_matlab",
    "knowledge_access_other",
    "experience_outside_role",
    "coding_ability_change",
    "prev_coding_experience",
    "first_learned",
    "heard_of_RAP",
    "RAP_champ",
    "know_RAP_champ",
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
    "prac_package",
    "prac_dir_structure",
    "prac_style",
    "prac_automated_QA",
    "prac_AQUA_book",
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
    "reproducible_workflow"
  )

  return(data)

}

#' @title API to enforce streaming rules (wave 3)
#'
#' @description Enforce all streaming rules across all questions
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data
#'
#' @export

w3_enforce_streaming <- function(data){

  if (class(data) != "data.frame") {
    stop("Unexpected input: data is not a data.frame.")}

  data <- w3_enforce_degree_streaming(data)
  data <- w3_enforce_code_freq_streaming(data)
  data <- w3_enforce_outside_role_streaming(data)
  data <- w3_enforce_prev_exp_streaming(data)
  data <- w3_enforce_heard_of_rap_streaming(data)
  data <- w3_enforce_rap_champ_streaming(data)

  return(data)
}


#' @title Enforce degree streaming (wave 3)
#'
#' @description Enforce the streaming rules that if highest qualification isn't degree then no degree subject has "Yes" returned
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_degree_streaming <- function(data){

  degree_data <- data[grepl("degree_", colnames(data))]
  degree_data[!is.na(data$highest_qualification) & !(data$highest_qualification %in% c("Bachelor's degree (or equivalent)","Master's degree (or equivalent)","Doctoral degree (or equivalent)")),] <- "No"
  data[colnames(degree_data)] <- degree_data

  return(data)
}

#' @title Enforce code frequency streaming (wave 3)
#'
#' @description Enforce the streaming rules that if code frequency is never then multiple questions are skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_code_freq_streaming <- function(data){

  data <- w3_enforce_coding_prac(data)
  data <- w3_enforce_doc(data)
  data <- w3_enforce_ci(data)
  data <- w3_enforce_dep_man(data)
  data <- w3_enforce_rep_wf(data)
  data <- w3_enforce_comment(data)

  return(data)

}

#' @title Enforce code frequency streaming on coding practices (wave 3)
#'
#' @description Enforce the streaming rules that if code frequency is never then coding practices is skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame
#'
#' @importFrom rlang .data

w3_enforce_coding_prac <- function(data){

  coding_prac_data <- dplyr::select(data, .data$prac_use_open_source:.data$prac_AQUA_book)
  coding_prac_data[!is.na(data$code_freq) & (data$code_freq %in% c("Never")),] <- NA
  data[colnames(coding_prac_data)] <- coding_prac_data

  return(data)
}

#' @title Enforce code frequency streaming on documentation questions (wave 3)
#'
#' @description Enforce the streaming rules that if code frequency is never then documentation practices is skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame
#'
#' @importFrom rlang .data

w3_enforce_doc <- function(data){

  doc_data <- dplyr::select(data, .data$doc_desk_notes:.data$doc_other)
  doc_data[!is.na(data$code_freq) & (data$code_freq %in% c("Never")),] <- NA
  data[colnames(doc_data)] <- doc_data

  return(data)
}

#' @title Enforce code frequency streaming on continuous integration questions (wave 3)
#'
#' @description Enforce the streaming rules that if code frequency is never then continuous integration questions is skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_ci <- function(data){

  data$CI[!is.na(data$code_freq) & (data$code_freq %in% c("Never"))] <- NA

  return(data)
}

#' @title Enforce code frequency streaming on dependency management questions (wave 3)
#'
#' @description Enforce the streaming rules that if code frequency is never then dependency management questions is skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_dep_man <- function(data){

  data$dependency_management[!is.na(data$code_freq) & (data$code_freq %in% c("Never"))] <- NA

  return(data)
}

#' @title Enforce code frequency streaming on reproducible workflow questions (wave 3)
#'
#' @description Enforce the streaming rules that if code frequency is never then reproducible workflow questions is skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_rep_wf <- function(data){

  data$reproducible_workflow[!is.na(data$code_freq) & (data$code_freq %in% c("Never"))] <- NA

  return(data)
}

#' @title Enforce code frequency streaming on extra comment question (wave 3)
#'
#' @description Enforce the streaming rules that if code frequency is never then extra comment question is skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_comment <- function(data){

  data$doc_comments[!is.na(data$code_freq) & (data$code_freq %in% c("Never"))] <- NA

  return(data)
}

#' @title Enforce coding experience outside current role streaming
#'
#' @description Enforce the streaming rules that if coding experience outside current role is no then follow up questions are skipped
#'
#' @param data pre-processed data
#'
#' @return data frame

w3_enforce_outside_role_streaming <- function(data){

  prev_exp_data <- dplyr::select(data, "coding_ability_change":"first_learned")
  prev_exp_data[!is.na(data$experience_outside_role) & (data$experience_outside_role %in% c("No")),] <- NA
  data[colnames(prev_exp_data)] <- prev_exp_data

  return(data)
}

#' @title Enforce previous coding experience streaming (wave 3)
#'
#' @description Enforce the streaming rules that if previous coding experience is no then where learned to code is skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_prev_exp_streaming <- function(data){

  data$first_learned[!is.na(data$prev_coding_experience) & (data$prev_coding_experience %in% c("No"))] <- NA

  return(data)
}

#' @title Enforce heard of RAP streaming (wave 3)
#'
#' @description Enforce the streaming rules that if heard of RAP is "No" then skip various RAP based questions
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_heard_of_rap_streaming <- function(data){

  data <- w3_enforce_rap_champ_knowl_streaming(data)
  data <- w3_enforce_outside_role_streaming(data)

  return(data)
}

#' @title Enforce heard of RAP streaming on RAP_champion and know_RAP_champ (wave 3)
#'
#' @description Enforce the streaming rules that if heard of RAP is "No" then are you RAP_champion and know_RAP_champ are skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_rap_champ_knowl_streaming <- function(data){

  rap_data <- data[,c("RAP_champ","know_RAP_champ")]
  rap_data[!is.na(data$heard_of_RAP) & (data$heard_of_RAP %in% c("No")),] <- NA
  data[colnames(rap_data)] <- rap_data

  return(data)
}

#' @title Enforce heard of RAP streaming on RAP statements (wave 3)
#'
#' @description Enforce the streaming rules that if heard of RAP is "No" then statements on RAP are skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_rap_state_streaming <- function(data){

  statement_data <- dplyr::select(data, "RAP_confident":"RAP_comments")
  statement_data[!is.na(data$heard_of_RAP) & (data$heard_of_RAP %in% c("No")),] <- NA
  data[colnames(statement_data)] <- statement_data

  return(data)
}

#' @title Enforce RAP champion streaming (wave 3)
#'
#' @description Enforce the streaming rules that if RAP champion is yes then Do you know your RAP champion is skipped
#'
#' @param data pre-processed data (wave 3)
#'
#' @return data frame

w3_enforce_rap_champ_streaming <- function(data){

  data$know_RAP_champ[!is.na(data$RAP_champion) & (data$RAP_champion %in% c("Yes"))] <- NA

  return(data)
}

