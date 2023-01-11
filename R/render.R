
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
#' @export

create_prof_pages <- function() {

  unlink("quarto/main/professions", recursive = TRUE)

  dir.create("quarto/main/professions")

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
    "government geography profession",
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

  template <- readr::read_file("quarto/templates/summary.qmd")

  # read_file replaces new lines with double new lines
  template <- gsub("\r", "", template)

  paths <- c()

  for (i in 1:length(prof_cols)) {
    filter <- glue::glue('data[data${prof_cols[[i]]} == "Yes", ]')

    title <- paste0("Profession summary: ", prof_names[[i]])

    # Custom open and close tags are used here to avoid clashes with quarto syntax
    contents <- glue::glue(template, .open = "{{{", .close = "}}}") %>% as.character()

    path <- paste0("quarto/main/professions", "/", filenames[[i]])

    write(contents, path)

    html_path <- gsub(".qmd", ".html", filenames[[i]])
    html_path <- paste0("professions/", html_path)

    paths <- append(paths, glue::glue("- [{to_upper_first(prof_names[[i]])}]({html_path})\n\n\n"))
  }

  prof_links <- paste0(paths, collapse = "")

  prof_page_template <- readr::read_file("quarto/templates/professions.qmd")

  # read_file replaces new lines with double new lines
  prof_page_template <- gsub("\r", "", prof_page_template)

  prof_page_contents <- glue::glue(prof_page_template, .open = "{{{", .close = "}}}") %>% as.character()

  write(prof_page_contents, "quarto/main/professions.qmd")

  return(paths)

}



