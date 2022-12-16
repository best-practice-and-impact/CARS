#' @title Create tidy cross table
#'
#' @description Returns a cross table in tidy data format.
#'
#' @param data pre-processed CARS data set
#' @param col1 first column of interest
#' @param col2 column to cross-tabulate first column against
#' @param levels1 factor levels for col1
#' @param levels2 factor levels for col2
#'
#' @return data.frame

create_tidy_cross_table <- function(data, col1, col2, levels1, levels2){

  selected_data <- data %>% dplyr::select(all_of(c(col1, col2)))

  selected_data[col1] <- factor(selected_data[[col1]], levels = levels1)

  selected_data[col2] <- factor(selected_data[[col2]], levels = levels2)

  frequencies <- selected_data %>%
    dplyr::count(across(c(col1, col2)), .drop=FALSE) %>%
    tidyr::drop_na() %>%
    data.frame

  return(frequencies)

}
