#' @title Data frame to html table
#'
#' @description converts dataframe to presentation HTML table
#'
#' @param data list of CARS frequency tables, table names should match config
#' @param config CARS config
#' @param question string, question of interest. Name should match config.
#' @param column_headers optional: list of column headers for the output table
#' @param crosstab whether to create a cross tabulation. FALSE by default
#' @param sample additionally returns sample size. TRUE by default
#' @return HTML table
#' @export

df_to_table <- function(data, config, question, crosstab = FALSE, column_headers, sample = TRUE) {



  if (!missing(config)){
    list2env(get_question_data(config, question), envir = environment())
      data <- data[[cols]]
  }

  table_data <- data

  table_data["n"] <- round(table_data["n"] * 100, 1) |> lapply(paste0, "%")

  if (crosstab) {
    table_data <- dplyr::select(table_data, !dplyr::any_of(c("count", "sample")))
    table_data <- df_to_crosstab(table_data)

    alignment <- c("l", rep("r", ncol(table_data)-1))

    if (missing(column_headers)) {
      column_headers <- colnames(table_data)
    }

  } else {
    alignment <- c(rep("l", ncol(table_data)-1), "r")
  }

  if (!missing(column_headers)) {
    colnames(table_data) <- column_headers
  } else {
    colnames(table_data) <- c(full_question, "Percentage", "Count", "Total")
  }


  table <- DT::datatable(table_data,
                         extensions = 'Buttons',
                         rownames = FALSE,
                         options = list(dom = 'tB',
                                        buttons = c('copy', 'csv', 'excel')
                                        )
  )



  return(table)
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

  data <- tidyr::pivot_wider(data, id_cols = 1, names_from = 2, values_from = 3) |>
    data.frame(check.names = FALSE)

  return(data)
}
