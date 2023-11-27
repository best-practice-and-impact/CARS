#' @title Derive variables
#'
#' @description API function for deriving additional variables.
#'
#' @param data tidied and relabelled CARS wave 3 dataset.
#'
#' @return data (data.frame).
#'
#' @export

derive_vars <- function(data) {
  data <- data %>%
    derive_language_status() %>%
    derive_rap_score()
    derive_rap_champ_status()

  return(data)
}



#' @title Derive language status
#'
#' @description Derive the status of each programmming language as "access" (access only), "knowledge" (knowledge only), "both" or "neither".
#'
#' @param data tidied CARS wave 3 data (data.frame).
#'
#' @return data.frame

derive_language_status <- function(data) {

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

derive_rap_score <- function(data){
  data <- derive_basic_rap_scores(data)
  data <- derive_advanced_rap_scores(data)

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

derive_basic_rap_scores <- function(data) {

  expected_columns <- c("code_freq",
                        "prac_use_open_source",
                        "prac_open_source_own",
                        "prac_version_control",
                        "prac_review",
                        "prac_development_QA",
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
                       "development_QA_score",
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

derive_advanced_rap_scores <- function(data) {

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
derive_rap_champ_status <- function(data){

  data <- data %>%
          mutate(RAP_champ_status = case_when(have_RAP_champ == "Yes" & know_RAP_champ == "Yes, and I am a RAP Champion" ~ "Yes, and I am a RAP Champion",
                                              have_RAP_champ == "Yes" & know_RAP_champ == "Yes" ~ "Yes, and I know who the RAP Champion is",
                                              have_RAP_champ == "Yes" & know_RAP_champ == "No" ~ "Yes, but I don't know who the RAP Champion is",
                                              have_RAP_champ == "No" ~ "No",
                                              have_RAP_champ == "I don't know" ~ "I don't know"))

}


