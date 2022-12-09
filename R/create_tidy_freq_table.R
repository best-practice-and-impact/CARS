#' @title Create tidy frequency table
#'
#' @description Returns a frequency table in tidy data format.
#'
#' @param data tidy CARS data set
#' @param questions columns to filter data on
#' @param responses all possible factor values in the filtered columns
#' @param labels labels to rename the column headers
#' @param labels default FALSE. orders data frame alphabetically by first column
#'
#' @return data.frame
#'
#' @export

create_tidy_freq_table <- function(data, questions, responses, labels, order=FALSE){

  labels_list <- as.list(labels)
  names(labels_list) <- questions

  selected_data <- data %>% dplyr::select(questions)

  selected_data[] <- lapply(selected_data, factor, levels = responses)

  selected_data <- selected_data[complete.cases(selected_data),]

  frequencies <- selected_data %>%
    tidyr::pivot_longer(cols=questions) %>%
    dplyr::group_by(name) %>%
    dplyr::count(value, .drop=FALSE) %>%
    dplyr::mutate(name = dplyr::recode(name, !!!labels_list))

  frequencies <- data.frame(frequencies)

  if(order){
    frequencies <- frequencies[tolower(order(frequencies$name)),]
  }

  return(frequencies)
}
