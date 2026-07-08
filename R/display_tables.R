#' @title Convert frequency data frame to HTML table
#'
#' @description
#' Creates an HTML table from frequency data, with optional crosstab formatting,
#' optional heatmap-style cell colouring for percentage values, and optional sample-size footnote.
#'
#' @param data Frequency data (data frame). Expected input includes columns such as
#'   category/question text, \code{n} (proportion), and optionally \code{count} and \code{sample}.
#' @param config CARS config object (optional). If supplied, \code{question} is used with
#'   \code{get_question_data()} to select relevant columns.
#' @param question Question key to display/select when \code{config} is provided.
#' @param crosstab Logical; if \code{TRUE}, formats output as a crosstab via \code{df_to_crosstab()}.
#' @param column_headers Optional character vector of column names to apply to the output table.
#' @param sample Logical; if \code{TRUE}, appends sample size as a table footnote.
#' @param heatmap Logical; if \code{TRUE}, applies heatmap colouring to percentage cells.
#' @param heatmap_palette Character vector of colours used for heatmap scaling
#'   (passed to \code{scales::col_numeric()}).
#' @param crosstab_global_scale Logical; used only when \code{crosstab = TRUE}.
#'   If \code{TRUE}, all crosstab value columns share one colour scale;
#'   if \code{FALSE}, each value column is scaled independently.
#' @param show_percent_symbol Logical; if \code{TRUE}, appends a percent symbol to displayed percentage values.
#'
#' @return A \code{kableExtra}/HTML table object.
#'
#' @details
#' The function removes \code{count} and \code{sample} from displayed columns, converts
#' \code{n} proportions to percentage labels, and applies alignment rules by table type.
#' Heatmap styling is applied at the table-cell level with \code{kableExtra::column_spec()}.
#'
#' @examples
#' \dontrun{
#' df_to_table(data = tables$code_freq)
#'
#' df_to_table(
#'   data = tables$code_freq,
#'   crosstab = TRUE,
#'   heatmap = TRUE
#' )
#' }
#'
#' @export

df_to_table <- function(data,
                        config,
                        question,
                        crosstab = FALSE,
                        column_headers,
                        sample = TRUE,
                        heatmap = FALSE,
                        heatmap_palette = c("#12436D", "#28A197", "#F46A25"),
                        crosstab_global_scale = TRUE,
                        percent = TRUE) {
  heatmap_col_css <- paste(
    "padding: 6px 10px !important;",
    "border-top: 0 !important;",
    "border-bottom: 0 !important;",
    "border-left: 0 !important;",
    "border-right: 0 !important;"
  )
  heatmap_row_css <- "border-top: 0 !important; border-bottom: 0 !important;"
  table_attr <- if (isTRUE(heatmap)) {
    'style="border-collapse: collapse; border-spacing: 0;"'
  } else {
    NULL
  }

  if (!missing(config)) {
    list2env(get_question_data(config, question), envir = environment())
    data <- data[[cols]]
  }

  table_data <- dplyr::select(data, !dplyr::any_of(c("count", "sample")))

  # Keep numeric copy for heatmap, show n as percent text
  n_pct <- round(table_data$n * 100, 1)
  missing_n <- is.na(n_pct)
  if (isTRUE(percent)) {
    table_data$n <- ifelse(missing_n, "", paste0(n_pct, "%"))
  } else {
    table_data$n <- ifelse(missing_n, "", as.character(n_pct))
  }

  if (isTRUE(crosstab)) {
    table_data <- df_to_crosstab(table_data)
    table_data <- dplyr::mutate(table_data, dplyr::across(-1, ~ ifelse(is.na(.x), "", as.character(.x))))
    alignment <- c("l", rep("r", ncol(table_data) - 1))

    if (missing(column_headers)) {
      column_headers <- colnames(table_data)
    }
  } else {
    alignment <- c(rep("l", ncol(table_data) - 1), "r")
  }

  if (!missing(column_headers)) {
    colnames(table_data) <- column_headers
  } else {
    colnames(table_data) <- c(full_question, "Percentage")
  }

  heatmap_backgrounds <- list()

  if (isTRUE(heatmap)) {
    if (isTRUE(crosstab)) {
      value_cols <- 2:ncol(table_data)
      num_df <- lapply(table_data[value_cols], function(x) {
        as.numeric(x)
      })

      if (isTRUE(crosstab_global_scale)) {
        global_domain <- range(unlist(num_df), na.rm = TRUE)
      }

      for (j in seq_along(value_cols)) {
        col_idx <- value_cols[j]
        vals <- num_df[[j]]
        domain <- if (isTRUE(crosstab_global_scale)) global_domain else range(vals, na.rm = TRUE)
        bg <- scales::col_numeric(palette = heatmap_palette, domain = domain)(vals)
        table_data[[col_idx]][table_data[[col_idx]] %in% c("NA", "NA%")] <- ""
        heatmap_backgrounds[[as.character(col_idx)]] <- bg
      }
    } else {
      pct_col <- ncol(table_data)
      pct_domain <- range(n_pct, na.rm = TRUE)
      bg <- scales::col_numeric(palette = heatmap_palette, domain = pct_domain)(n_pct)
      table_data[[pct_col]][table_data[[pct_col]] %in% c("NA", "NA%")] <- ""
      heatmap_backgrounds[[as.character(pct_col)]] <- bg
    }
  }

  html <- knitr::kable(
    table_data,
    align = alignment,
    format = "html",
    escape = TRUE,
    table.attr = table_attr
  ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      bootstrap_options = "condensed"
    ) |>
    kableExtra::column_spec(1, extra_css = "white-space: nowrap;")

  if (isTRUE(heatmap)) {
    heatmap_cols <- if (isTRUE(crosstab)) 2:ncol(table_data) else ncol(table_data)

    for (col_idx in heatmap_cols) {
      html <- kableExtra::column_spec(
        html,
        col_idx,
        background = heatmap_backgrounds[[as.character(col_idx)]],
        color = "white",
        extra_css = heatmap_col_css
      )
    }

    html <- kableExtra::row_spec(
      html,
      1:nrow(table_data),
      extra_css = heatmap_row_css
    )
  }

  if (isTRUE(sample)) {
    html <- kableExtra::add_footnote(
      html,
      paste0("Sample size = ", data$sample[1]),
      notation = "none"
    )
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

  data <- tidyr::pivot_wider(data, id_cols = 1, names_from = 2, values_from = 3) |>
    data.frame(check.names = FALSE)

  return(data)
}
