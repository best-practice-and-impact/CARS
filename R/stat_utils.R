
#' @title Get percentage and confidence interval
#'
#' @param table frequency table with sample column
#' @param freq_col frequency column index
#' @param n_col sample column index
#'
#' @return data frame with new percentage and upper/lower ci columns
#' @export

get_ci <- function(table, freq_col, n_col) {
  table[c("percent", "lower", "upper")] <- Hmisc::binconf(table[[freq_col]], table[[n_col]], method = "wilson")
  table$lower_ci <- table$percent - table$lower
  table$upper_ci <- table$upper - table$percent

  return(table)
}

