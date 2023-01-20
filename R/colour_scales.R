#' @title Create 2 colour gradient palette
#'
#' @description Creates a list of  colour names (RGB). Generates a palate using two colour gradients and an optional grey neutral colour.
#' Each half of the palette contains different shades of the colour submitted to the function. These are useful where two colour
#' scales in the same palette are needed, e.g. likert type plots. By default, the main colours are shades of blue and orange
#' used in the analysis function colour scheme.
#'
#' @param n the number of colours needed
#' @param colour1 the first colour in the scale - a numeric vector representing red, green and blue (max 255)
#' @param colour2 the last colour in the scale - numeric vector representing red, green and blue (max 255).
#' @param mid the middle value of the scale. If neutral_mid = TRUE this value will be represented in grey. Otherwise, this will be
#' the endpoint of the first colour gradient (e.g. if n=6 and mid=3, there will be three shades of each colour)
#' @param neutral_mid whether the middle of the scale should be represented in grey (logical, TRUE by default)
#' @param colour_names whether to return HTML colour names. TRUE by default.
#'
#' @return RGB colours
#'
#' @export


get_2colour_gradients <- function(n, colour1 = c(0, 69, 86), colour2 = c(255, 105, 0), mid, neutral_mid = TRUE, colour_names = TRUE) {

  if (!is.numeric(n) | length(n) > 1) {
    stop("n is not a numeric value")
  } else if (!n >= 2) {
    stop("Unexpected input - n should be >=2")
  }

  # Find scale midpoint if not specified
  if (missing(mid)) {
    mid <- ceiling((n + 1) / 2)
  } else if (mid > n-1 | mid == 0) {
    stop("Unexpected input: mid should not be greater than n-1 or smaller than 1")
  }

  if (class(colour1) != "numeric" | class(colour2) != "numeric") {
    stop("colours are not a vector of three integers")
  } else if (length(colour1) != 3 | length(colour2) != 3) {
    stop("colours must contain three values")
  } else if (!missing(mid) & class(mid) != "numeric") {
    stop("mid is not a numeric value")
  } else if (typeof(neutral_mid) != "logical") {
    stop("neutral_mid is not a logical value")
  }

  # Set number of shades per colour
  if (n == 2){
    c1_shades <- 1
    c2_shades <- 1
    neutral_mid <- FALSE
  } else {
    c1_shades <- ifelse(neutral_mid, mid - 1, mid)
    c2_shades <- floor(n - mid)
  }

  c1_gradient <- get_gradient(c1_shades, colour = colour1, colour_names = FALSE)
  c2_gradient <- rev(get_gradient(c2_shades, colour = colour2, colour_names = FALSE))

  # Calculate brightness for neutral middle
  if (neutral_mid) {
    c1_last <- utils::tail(c1_gradient, n = 1)
    c1_brightness <- (max(c1_last[[1]]) + min (c1_last[[1]])) / 2

    c2_first <- c2_gradient[1]
    c2_brightness <- (max(c2_first[[1]]) + min (c2_first[[1]])) / 2

    mid_brightness <- 175

    mid_colour <- c(mid_brightness, mid_brightness, mid_brightness)
    colours <- c(c1_gradient, list(mid_colour), c2_gradient)
  } else {
    colours <- c(c1_gradient, c2_gradient)
  }

  if (colour_names) {
    colours <- lapply(colours, function(x) grDevices::rgb(x[1], x[2], x[3], max = 255)) %>% unlist
  }

  return(colours)

}


#' @title Create 2 colour scale palette
#'
#' @description Creates a list of  colour names (RGB). Generates a scale between two colours. By default these are the
#' shades of orange and blue used in the analysis function colour scheme.
#'
#' @param n the number of colours needed
#' @param colour1 the first colour in the scale - a numeric vector representing red, green and blue (max 255)
#' @param colour2 the last colour in the scale - numeric vector representing red, green and blue (max 255).
#' @param colour_names whether to return HTML colour names. TRUE by default.
#'
#' @return vector of hexadecimal colours containing shades between the two selected colours
#'
#' @export

get_2colour_scale <- function(n, colour1 = c(0, 69, 86), colour2 = c(255, 105, 0), colour_names = TRUE) {

  if (!is.numeric(n) | length(n) > 1) {
    stop("n is not a numeric value")
  } else if (n < 2) {
    stop("Unexpected value - n should be > 2")
  } else if (class(colour1) != "numeric" | class(colour2) != "numeric") {
    stop("colours are not a vector of three integers")
  } else if (length(colour1) != 3 | length(colour2) != 3) {
    stop("colours must contain three values")
  }

  if (n != 2) {
    step  <-  (colour2 - colour1) / (n - 1)
    colours <- unname(
      lapply(c(1:(n - 1)),
             function(x){
               step * x + colour1
             }
      )
    )
    colours <- c(list(colour1), colours)
  } else {
    colours <- list(colour1, colour2)
  }

  if (colour_names) {
    colours <- lapply(colours, function(x) grDevices::rgb(x[1], x[2], x[3], max = 255)) %>% unlist
  }

  return(colours)

}


#' @title Create single colour gradient
#'
#' @description Creates a list of colours for plotting. The list contains n shades of the original colour, increasing in brightness.
#'
#' @param n the number of colours needed
#' @param colour the first colour of the gradient - numeric vector with three values representing red, blue and green (max 255)
#' @param colour_names whether to return HTML colour names. TRUE by default.
#'
#' @return RGB colours
#'
#' @export

get_gradient <- function(n, colour = c(0, 69, 86), colour_names = TRUE) {

  if (!is.numeric(n) | length(n) > 1) {
    stop("Unexpected value - n is not a numeric value")
  } else if (!is.numeric(colour) | length(colour) != 3) {
    stop("colour is not a vector of three integers")
  }

  if (n != 1){
    # Calculate lighter shade of original colour
    c2 <- colour + (255 - colour) * 0.5

    step <- (c2 - colour) / (n - 1)

    colours <- unname(
      lapply(c(1:(n - 1)),
             function(x){
               step * x + colour
             })
    )

    colours <- append(list(colour), colours)
  } else {
    colours <- list(colour)
  }

  if (colour_names) {
    colours <- lapply(colours, function(x) grDevices::rgb(x[1], x[2], x[3], max = 255)) %>% unlist
  }

  return(colours)
}


#' @title create 3 colour palette
#'
#' @description create a list of 3 colours in RGB; orange, blue and green for plotting
#'
#' @param n the number of colours needed
#' @param colour_names whether to return HTML colour names. TRUE by default.
#'
#' @return RGB colours
#'
#' @export

get_3colour_scale <- function(n, colour_names = TRUE) {

  if (!is.numeric(n) | length(n) > 1) {
    stop("n is not a numeric value")
  } else if (n != 3) {
    stop("Unexpected value - n should be = 3")
  }

  colours <- list(c(255,105,0), c(0, 69, 86), c(255, 211, 0))

  if (colour_names) {
    colours <- lapply(colours, function(x) grDevices::rgb(x[1], x[2], x[3], max = 255)) %>% unlist
  }

  return(colours)
}
