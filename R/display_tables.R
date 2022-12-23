#' @title Data frame to html table
#'
#' @description converts dataframe to presentation HTML table
#'
#' @param data frequency table in tidy format. Last column should contain proportions.
#' First one or two columns should contain factors.
#' @param column_headers optional: list of column headers for the output table
#' @param crosstab whether to create a cross tabulation. FALSE by default
#'
#' @return HTML table
#' @export

df_to_table <- function(data, column_headers, crosstab = FALSE) {

  proportion_col <- length(data)

  data[proportion_col] <- paste0(round(data[[proportion_col]] * 100, 1), "%")

  if (crosstab) {
    data <- df_to_crosstab(data)
  }

  if (!missing(colnames)) {
    colnames(data) <- column_headers
  }

  html <- knitr::kable(data, format = "html")

  return(html)
}

#' @title Convert data frame to crosstab
#'
#' @description Converts tidy data with two factor columns to a cross tabulation (wide data).
#'
#' @param data tidy frequency table
#'
#' @return wide data.frame

df_to_crosstab <- function(data) {
  if (ncol(data) != 3) {
    stop("Unexpected input: expecting a dataframe with three columns.")
  }

  data <- tidyr::pivot_wider(data, id_cols = 1, names_from = 2, values_from = 3) %>% data.frame(check.names = FALSE)

  return(data)
}
