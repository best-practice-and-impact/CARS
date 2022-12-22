#' Derive variables
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

  return(data)
}



#' Derive language status
#'
#' @description Derve the status of each programmming language as "access" (access only), "knowledge" (knowledge only), "both" or "neither".
#'
#' @param data tidied CARS wave 3 data (data.frame).
#'
#' @return data.frame

derive_language_status <- function(data) {
  lang_list <- colnames(data)[grepl("access_", colnames(data))]

  lang_list <- lang_list[!grepl("other", lang_list)]

  lang_list <- gsub("access_", "", lang_list)

  new_vars <- sapply(lang_list, function(lang) {
    access_col <- data[paste0("access_", lang)]

    knowledge_col <- data[paste0("knowledge_", lang)]

    dplyr::case_when(access_col == "Yes" & knowledge_col == "Yes" ~ "both",
                     access_col == "Yes" & knowledge_col != "Yes" ~ "access",
                     access_col != "Yes" & knowledge_col == "Yes" ~ "knowledge",
                     access_col != "Yes" & knowledge_col != "Yes" ~ "neither")
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


#'@title Derive basic RAP scores
#'
#'@description Derive basic RAP score columns from existing variables and add to the dataframe.
#'
#'@param data a date frame containing cleaned CARS wave 3 data
#'
#'@return dataframe containing the additional basic RAP score columns
#'

derive_basic_rap_scores <- function(data) {

  expected_columns <- c("prac_use_open_source",
                        "prac_open_source_own",
                        "prac_version_control",
                        "prac_review",
                        "prac_AQUA_book",
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

  high_vals <- c("Regularly", "All the time")

  data$use_open_source_score <- ifelse(data$prac_use_open_source %in% high_vals, 1, 0)
  data$open_code_score <- ifelse(data$prac_open_source_own %in% high_vals, 1, 0)
  data$version_control_score <- ifelse(data$prac_version_control %in% high_vals, 1, 0)
  data$peer_review_score <- ifelse(data$prac_review %in% high_vals, 1, 0)
  data$AQUA_book_score <- ifelse(data$prac_AQUA_book %in% high_vals, 1, 0)
  data$doc_score <- ifelse(data$doc_readme %in% high_vals & data$doc_comments %in% high_vals, 1, 0)

  data$basic_rap_score <- rowSums(data[,c("use_open_source_score",
                                          "open_code_score",
                                          "version_control_score",
                                          "peer_review_score",
                                          "AQUA_book_score",
                                          "doc_score")])
  return(data)

}


#'@title Derive advanced RAP scores
#'
#'@description Derive advanced RAP score columns from existing variables and add to the dataframe.
#'
#'@param data a date frame containing cleaned CARS wave 3 data
#'
#'@return dataframe containing the additional advanced RAP score columns
#'

derive_advanced_rap_scores <- function(data) {

  expected_columns <- c("prac_functions",
                        "prac_unit_test",
                        "doc_functions",
                        "prac_package",
                        "prac_style",
                        "prac_automated_QA",
                        "prac_dir_structure")

  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  } else if (!all(expected_columns %in% colnames(data))) {
    missing <- paste(expected_columns[!(expected_columns %in% colnames(data))], collapse = "\n")
    stop(
      paste0("Unexpected input - missing column names: ", missing)
    )
  }

  high_vals <- c("Regularly", "All the time")

  data$function_score <- ifelse(data$prac_functions %in% high_vals, 1, 0)
  data$unit_test_score <- ifelse(data$prac_unit_test %in% high_vals, 1, 0)
  data$function_doc_score <- ifelse(data$doc_functions %in% high_vals, 1, 0)
  data$package_score <- ifelse(data$prac_package %in% high_vals, 1, 0)
  data$code_style_score <- ifelse(data$prac_style %in% high_vals, 1, 0)
  data$cont_integreation_score <- ifelse(data$prac_automated_QA == "Yes", 1, 0)
  data$dep_management_score <- ifelse(data$prac_dir_structure == "Yes", 1, 0)

  data$advanced_rap_score <- rowSums(data[,c("function_score",
                                             "unit_test_score",
                                             "function_doc_score",
                                             "package_score",
                                             "code_style_score",
                                             "cont_integreation_score",
                                             "dep_management_score")])
  return(data)

}
