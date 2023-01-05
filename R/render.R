
#' @title render site
#'
#' @description render CARS publication from quarto
#'
#' @param data full pre-processed CARS dataset
#' @param path quarto input
#' @param output_path output path (will overwrite existing outputs). Should match the path set in the quarto site yaml
#'
#' @export

render_site <- function(data, path = "quarto/main", output_path = "docs/") {
  unlink(output_path, recursive = TRUE)

  tables <- CARS::summarise_all(data, all_tables = TRUE)

  samples <- CARS::sample_sizes(data)

  dir.create(paste0(path, "/temp"))
  save(tables, samples, file = paste0(path, "/temp/summary_tables.Rda"))

  quarto::quarto_render(input = path, as_job = FALSE)

  unlink(paste0(path, "/temp"), recursive = TRUE)
}



#' @title render profession page
#'
#' @description render CARS publication from quarto
#'
#' @param data full pre-processed CARS dataset
#' @param prof profession to include in the report
#' @param title page title
#' @param output_path output path (will overwrite existing outputs)
#' @param path quarto input
#'
#' @export

render_prof_page <- function(data, prof, title, output_path, path = "quarto/template") {

  data <- data[data[prof] == "Yes", ]

  tables <- CARS::summarise_all(data, all_tables = TRUE)

  samples <- CARS::sample_sizes(data)

  dir.create(paste0(path, "/temp"))
  save(tables, samples, file = paste0(path, "/temp/", "summary_tables.Rda"))

  quarto::quarto_render(input = paste0(path, "/summary.qmd"), output_file = output_path, as_job = FALSE, execute_params = list("title" = title))

  unlink(paste0(path, "/temp"), recursive = TRUE)
}
