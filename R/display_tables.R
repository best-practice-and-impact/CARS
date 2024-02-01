#' @title Data frame to html table
#'
#' @description converts dataframe to presentation HTML table
#'
#' @param data frequency table in tidy format. Last column should contain proportions.
#' First one or two columns should contain factors.
#' @param column_headers optional: list of column headers for the output table
#' @param n optional: sample size
#' @param crosstab whether to create a cross tabulation. FALSE by default
#' @param proportion_col optional: columns to be turned into a percentage. Last column only be default
#' @param sample optional: add sample size as additional column
#' @return HTML table
#' @export

df_to_table <- function(data, column_headers, n, crosstab = FALSE, proportion_col, sample) {

  if (missing(proportion_col)) {
    proportion_col <- length(data)
  }

  data[proportion_col] <- round(data[proportion_col] * 100, 1) %>% lapply(paste0, "%")

  if (crosstab) {
    data <- df_to_crosstab(data)

    if (!missing(sample)) {
      data <- dplyr::mutate(data, Sample = sample)
    }

    alignment <- c("l", rep("r", ncol(data)-1))
  } else {

    if (!missing(sample)) {
      data <- dplyr::mutate(data, Sample = sample)
    }

    alignment <- c(rep("l", ncol(data)-1), "r")
  }

  if (!missing(column_headers)) {
    colnames(data) <- column_headers
  }

  html <- knitr::kable(data, align = alignment, format = "html") %>% kableExtra::kable_styling()

  if (!missing(n)) {
    html <- kableExtra::add_footnote(html, paste0("Sample size = ", n), notation = "none")
  }

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
