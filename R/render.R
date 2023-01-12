
#' @title render site
#'
#' @description render CARS publication from quarto
#'
#' @param data full pre-processed CARS dataset
#' @param path quarto input
#' @param path execute directory
#' @param output_path output path (will overwrite existing outputs). Should match the path set in the quarto site yaml
#'
#' @export

render_site <- function(data, path = "quarto/main", output_path = "docs/") {
  unlink(output_path, recursive = TRUE)

  dir.create(paste0(path, "/temp"))
  save(data, file = paste0(path, "/temp/data.rda"))

  # executes in higher directory level to avoid issues with .quarto stopping package from building
  quarto::quarto_render(input = path, as_job = FALSE)

  unlink(paste0(path, "/temp"), recursive = TRUE)
}


#' @title create filtered pages
#'
#' @description Creates summary statistics pages for professions/departments. Also creates index pages.
#'
#' @param data full, cleaned CARS dataset. Only needed for department pages.
#' @param type type of filtered page - department or profession
#' @param qmd_path qmd site folder (without trailing "/")
#' @param template_path template folder (without trailing "/")
#'
#' @export

create_filtered_pages <- function(data, type = c("professions", "departments"),
                                  qmd_path = "quarto/main",
                                  template_path = "quarto/templates") {

  type <- match.arg(type)

  if (missing(data) & type == "departments") {
    stop("Missing input: the cleaned CARS dataset is needed to generate department pages")
  }

  filtered_pages_path <- paste0(qmd_path, "/", type)

  unlink(filtered_pages_path, recursive = TRUE)

  dir.create(filtered_pages_path)

  if (type == "professions") {
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

    n_pages <- length(prof_cols)
  } else if (type == "departments") {
    dep_freqs <- table(data$department)
    dep_list <- names(dep_freqs[dep_freqs >= 20])

    filenames <- gsub("[[:punct:]]", " ", dep_list) %>% tolower()
    filenames <- gsub(" ", "-", filenames)
    filenames <- gsub("--", "-", filenames)
    filenames <- paste0(filenames, ".qmd")

    n_pages <- length(dep_list)
  }

  template <- readr::read_file("quarto/templates/summary.qmd")

  # read_file replaces new lines with double new lines
  template <- gsub("\r", "", template)

  paths <- c()

  for (i in 1:n_pages) {
    if (type == "professions") {
      filter <- glue::glue('data[!is.na(data${prof_cols[[i]]}) & data${prof_cols[[i]]} == "Yes", ]')

      title <- paste0("Profession summary: ", prof_names[[i]])
    } else if (type == "departments") {
      filter <- glue::glue('data[!is.na(data$department) & data$department == "{dep_list[i]}", ]')

      title <- paste0("Department summary: ", dep_list[i])
    }

     # Custom open and close tags are used here to avoid clashes with quarto syntax
    contents <- glue::glue(template, .open = "{{{", .close = "}}}") %>% as.character()

    path <- paste0(filtered_pages_path, "/", filenames[[i]])

    write(contents, path)

    html_path <- gsub(".qmd", ".html", filenames[[i]])
    html_path <- paste0(type, "/", html_path)

    if (type == "professions") {
      paths <- append(paths, glue::glue("- [{to_upper_first(prof_names[[i]])}]({html_path})\n\n\n"))
    } else if (type == "departments") {
      paths <- append(paths, glue::glue("- [{to_upper_first(dep_list[i])}]({html_path})\n\n\n"))
    }

  }

  links <- paste0(paths, collapse = "")

  link_page_template <- readr::read_file(paste0(template_path, "/", type, ".qmd"))

  # read_file replaces new lines with double new lines
  link_page_template <- gsub("\r", "", link_page_template)

  link_page_contents <- glue::glue(link_page_template, .open = "{{{", .close = "}}}") %>% as.character()

  write(link_page_contents, paste0(qmd_path, "/", type, ".qmd"))

}
