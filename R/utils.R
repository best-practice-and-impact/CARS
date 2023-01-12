#' @title Capitalise the first character of a string
#'
#' @param text a character object of any length
#'
#' @return character object, with the first character of each string capitalised
#'
#' @export

to_upper_first <- function(text) {
  characters <- stringr::str_split(text, pattern = "")

  sapply(characters, function(x) {
    x[[1]] <- toupper(x[[1]])
    paste0(x, collapse = "")
  })
}
