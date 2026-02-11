#' @title Derive variables
#'
#' @description function for deriving additional variables.
#'
#' @param data tidied and relabelled CARS wave 3 dataset.
#'
#' @return data (data.frame).
#'
#' @export

derive_vars <- function(data) {
  data <- data %>%
    derive_language_status() %>%
    derive_basic_rap_scores() %>%
    derive_advanced_rap_scores()

  return(data)
}



#' @title Derive language status
#'
#' @description Derive the status of each programmming language as "use" (use only), "knowledge" (knowledge only), "both" or "neither".
#'
#' @param data tidied CARS wave 3 data (data.frame).
#'
#' @return data.frame
#' @export

derive_language_status <- function(data) {

  lang_list <- colnames(data)[grepl("use_", colnames(data))]

  lang_list <- lang_list[!grepl("other", lang_list)]

  lang_list <- gsub("use_", "", lang_list)

  new_vars <- sapply(lang_list, function(lang) {
    use_col <- data[[paste0("use_", lang)]]

    knowledge_col <- data[[paste0("knowledge_", lang)]]

    dplyr::case_when(use_col == "Yes" & knowledge_col == "Yes" ~ "Both",
                     use_col == "Yes" & knowledge_col != "Yes" ~ "Use Only",
                     use_col != "Yes" & knowledge_col == "Yes" ~ "Knowledge Only",
                     use_col != "Yes" & knowledge_col != "Yes" ~ "Neither")
  })

  colnames(new_vars) <- paste0("status_", lang_list)

  return(data.frame(data, new_vars))
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
                        "prac_open_source",
                        "work_publish_code",
                        "work_git",
                        "prac_review",
                        "work_qa",
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
                       "qa_score",
                       "doc_score")

  high_vals <- c("Regularly", "Always")

  prac_cols <- expected_columns[!(expected_columns %in% c("code_freq"))]

  data <- data %>%
    dplyr::mutate(dplyr::across(.cols = expected_columns[expected_columns != "code_freq"],
                  ~ dplyr::case_when(code_freq == "Never" ~ NA_real_,
                              .x %in% high_vals ~ 1,
                              TRUE ~ 0),
                  .names = "{.col}_score")) %>%
    dplyr::rename_with(~ score_col_names[which(paste0(prac_cols, "_score") == .x)],
                .cols = paste0(prac_cols,
                               "_score")) %>%
    dplyr::mutate(basic_rap_score = rowSums(dplyr::across(dplyr::all_of(score_col_names))))

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
                        "prac_manual_tests",
                        "prac_auto_tests",
                        "doc_function",
                        "prac_control_flow",
                        "prac_config",
                        "prac_code_style",
                        "doc_dependencies")

  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  } else if (!all(expected_columns %in% colnames(data))) {
    missing <- paste(expected_columns[!(expected_columns %in% colnames(data))], collapse = "\n")
    stop(
      paste0("Unexpected input - missing column names: ", missing)
    )
  }

  score_col_names <- c("function_score",
                       "manual_test_score",
                       "auto_test_score",
                       "function_doc_score",
                       "control_flow_score",
                       "config_score",
                       "code_style_score",
                       "dep_management_score")

  high_vals <- c("Regularly", "Always")

  data <- data %>%
    dplyr::mutate(dplyr::across(.cols = expected_columns[expected_columns != "code_freq"],
                  ~ dplyr::case_when(code_freq == "Never" ~ NA_real_,
                              .x %in% high_vals ~ 1,
                              TRUE ~ 0),
                  .names = "{.col}_score")) %>%
    dplyr::rename_with(~ score_col_names[which(paste0(expected_columns[expected_columns != "code_freq"], "_score") == .x)],
                .cols = paste0(expected_columns[expected_columns != "code_freq"],
                               "_score")) %>%
    dplyr::mutate(advanced_rap_score = rowSums(dplyr::across(dplyr::all_of(score_col_names))))

  return(data)

}

