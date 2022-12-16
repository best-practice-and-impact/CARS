#' @title Create tidy frequency table
#'
#' @description Returns a frequency table in tidy data format.
#'
#' @param data pre-processed CARS data set
#' @param questions columns to filter data on
#' @param levels all possible factor values in the filtered columns
#' @param labels labels to rename the column headers
#'
#' @return data.frame

create_tidy_freq_table <- function(data, questions, levels, labels){

  labels_list <- as.list(labels)
  names(labels_list) <- questions

  selected_data <- data %>% dplyr::select(all_of(questions))

  selected_data[] <- lapply(selected_data, factor, levels = levels)

  frequencies <- selected_data %>%
    tidyr::pivot_longer(cols=questions) %>%
    dplyr::group_by(name) %>%
    dplyr::count(value, .drop=FALSE) %>%
    dplyr::mutate(name = dplyr::recode(name, !!!labels_list)) %>%
    dplyr::arrange(name, by_group=TRUE) %>%
    tidyr::drop_na() %>%
    data.frame

  return(frequencies)
}
