#'@title Read RMarkdown site yaml
#'
#'@description Read the rmarkdown site yaml and return list of navbar arguments. Used to render the custom navigation bar. The yaml file must follow the same syntax expected by RMarkdown/quarto.
#'
#'@param filename the yaml file name (including path)
#'
#'@return list of arguments
#'
#'@export

read_site_yml <- function(filename) {

  yml <- yaml::read_yaml(filename)


  # Find relevant fields
  if (is.null(yml$navbar$title) | is.null(yml$navbar$left)) {
    stop("Required fields missing in yaml file.")
  } else {
    navbar_info <- list(
      title = yml$navbar$title,
      pages = yml$navbar$left
    )

    return(navbar_info)
  }

}
