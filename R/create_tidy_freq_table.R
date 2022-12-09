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

  labels <- as.list(labels)
  names(labels) <- questions

  selected_data <- data %>% dplyr::select(questions)

  selected_data[] <- lapply(selected_data, factor, levels = responses)

  frequencies <- selected_data %>%
    tidyr::pivot_longer(cols=questions) %>%
    dplyr::group_by(name) %>%
    dplyr::count(value, .drop=FALSE) %>%
    dplyr::mutate(name = dplyr::recode(name, !!!labels))

  frequencies <- data.frame(frequencies)

  frequencies <- frequencies[complete.cases(frequencies),]

  if(order){
    frequencies <- frequencies[order(frequencies$name),]
  }

  return(frequencies)
}
