
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

  dir.create(paste0(path, "/temp"))
  save(data, file = paste0(path, "/temp/data.rda"))

  quarto::quarto_render(input = path, as_job = FALSE)

  unlink(paste0(path, "/temp"), recursive = TRUE)
}


#' @title Create profession page
#'
#' @description Create profession breakdown pages from template
#'
#' @param template_path location of the quarto template
#' @param output_path output folder. Do not end with "\". Proceed with caution:
#' existing folders will be overwritten, including any files
#'
#'
#' @export

create_prof_pages <- function(template_path = "quarto/templates/summary.qmd", output_path = "quarto/main/professions") {

  unlink(output_path, recursive = TRUE)

  dir.create(output_path)

  prof_cols <- c(
    "prof_DS",
    "prof_DDAT",
    "prof_GAD",
    "prof_GES",
    "prof_geog",
    "prof_GORS",
    "prof_GSR",
    "prof_GSG"
  )

  prof_names <- c(
    "government data scientists",
    "digital and data profession (DDAT)",
    "government actuary's department (GAD)",
    "government economic service (GES)",
    "governmebt geography profession",
    "government operational research (GORS)",
    "government social research (GSR)",
    "government statician group (GSG)"
  )

  filenames <- c(
    "data-scientists.qmd",
    "digital-and-data.qmd",
    "government-actuarys-department.qmd",
    "government-economic-service.qmd",
    "government-geography.qmd",
    "government-operational-research.qmd",
    "government-social-research.qmd",
    "government-statician-group.qmd"
  )

  template <- readr::read_file(template_path)

  # read_file replaces new lines with double new lines
  template <- gsub("\r", "", template)

  for (i in 1:length(prof_cols)) {
    filter <- glue::glue('data[data${prof_cols[[i]]} == "Yes", ]')

    title <- paste0("Profession summary: ", prof_names[[i]])

    # Custom open and close tags are used here to avoid clashes with quarto syntax
    contents <- glue::glue(template, .open = "{{{", .close = "}}}") %>% as.character()

    path <- paste0(output_path, "/", filenames[[i]])

    write(contents, path)
  }

}



