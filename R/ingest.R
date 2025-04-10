
#' @title Get tidy data from API
#'
#' @description API function for data ingest. Loads data from the smartsurvey API and convert to a tidy data frame.
#'
#' @param ... optional: arguments to be passed to \link{ingest}
#'
#' @return tidied CARS dataset
#'
#' @export

get_tidy_data_api <- function(...) {

  ingest(...) # First API request always fails so the API is pinged twice to circumvent this.

  data <- ingest(...) |>
    convert_raw() |>
    tidy_colnames()

  return(data)
}


#' @title Get tidy data from file
#'
#' @description API function for data ingest. Loads data from csv and returns pre-processed data frame.
#'
#' @param ... optional: arguments to be passed to \link{ingest}
#'
#' @return tidied CARS dataset
#'
#' @export

get_tidy_data_file <- function(...) {

  data <- read_file(...) |>
    tidy_colnames()

  return(data)
}

#' @title Ingest smartsurvey data
#'
#' @description Download smartsurvey export via the API. Download the exported data from smartsurvey using the API. Use convert_raw() to convert the API response to a data.frame.
#' Note: the first API request in a session will typically fail.
#'
#' @param survey the survey ID (character string/numeric). Defaults to "1167489".
#' @param token the API token (character string). Loaded from environment variable by default.
#' @param secret the secret API token (character string). Loaded from environment variable by default.
#' @param proxies proxy addresses (string). Loads from the user environment by default. Expects a string in the format "ip.address:port; alt.ip.address:port".
#' @param export the export ID (character string/numeric).

#' @return the exported data as a dataframe

ingest <- function(survey = "1376897",
                   token = Sys.getenv("CARS_TOKEN"),
                   secret = Sys.getenv("CARS_SECRET"),
                   proxies = Sys.getenv("alt_proxy"),
                   export) {

  if (missing(export)) {
    export <- "latest"
  }

  # Check input types
  if (!is.character(survey) && !is.numeric(survey) | !is.character(export) && !is.numeric(export)) {
    stop("Unexpected input - survey and export should lbe character or numeric variables.")
  }

  if (!is.character(token) | !is.character(secret)) {
    stop("Unexpected input - token and secret should be character variables.")
  }

  if (length(survey) > 1 | length(token) > 1 | length(token) > 1 | length(secret) > 1 ) {
    stop("Unexpected input - one or more of the supplied arguments contain multiple elements.")
  }

  # API request
  url <- paste0("https://api.smartsurvey.io/v1/surveys/", survey, "/exports/", export, "/download")

  if (!is.null(proxies)) {
    proxy_list <- stringr::str_split(proxies, ";")[[1]]

    for (proxy_address in proxy_list) {
      httr::set_config(httr::use_proxy(proxy_address))

      tryCatch(
        {
          r <- httr::GET(
            url,
            httr::authenticate(user = token, password = secret, type = "basic")
          )
        },
        error = function(e) {
          warning(paste("Error in API request, trying alternative proxy: ", e))
        }
      )

      if (exists("r") && r$status_code == 200) {
        break
      }

    }

  }

  return(r)
}

#' Read file
#'
#' Wrapper for read.csv - loads data using path saved in environment variable.
#'
#' @param name file name
#' @param dir data folder (defaults to CARS_DATA_DIR environment vairable)
#'
#' @return unprocessed CARS data
#' @export

read_file <- function(name, dir = Sys.getenv("CARS_DATA_DIR")) {
  read.csv(paste0(dir, "/", name), encoding = "utf-8")
}

#' @title Convert raw data to data frame
#'
#' @description Convert raw smartsurvey data to data.frame . Extract contents (raw csv) from smartsurvey API request and convert to data.frame
#'
#' @param r api response object
#'
#' @return response content as a data.frame

convert_raw <- function(r) {

  if (class(r) != "response") {
    stop("Unexpected input - r is not a response object.")
  } else if (r$status_code != 200) {
    stop("Unsuccessful API request - no data.")
  }

  content <- rawToChar(r$content)

  data <- utils::read.table(
    text = content,
    sep = ",",
    header = TRUE,
    fill = TRUE,
    quote = c("\"\"", "'"),
    na.strings = c("", ".", "NA", "-", "\"\"", "\".\"", "\"NA\"", "\"-\"")
  )

  # Fix apostrophes
  data[] <- lapply(data, function(x) gsub("@SQ@", "'", x))

  return(data)
}


#' @title Tidy column names
#'
#' @description Takes smartsurvey data as an input and converts to a tidier format, with question names removed from value rows.
#'
#' @param raw_data data frame returned by convert_raw()
#'
#' @return data frame with empty rows removed and tidied column names
#'
#' @export

tidy_colnames <- function(raw_data) {

  colnames <- rbind(colnames(raw_data), raw_data[1, ], raw_data[2, ])

  new_colnames <- apply(colnames, 2, function(x) {
    x[grep("X.", x)] <- NA

    paste0(x[!is.na(x)], collapse = ":")
  })

  output <- raw_data[3:nrow(raw_data), ]
  colnames(output) <- new_colnames

  rownames(output) <- NULL

  return(output)
}

#' @title Get all CARS data, including previous waves
#'
#' @description Ingest and preprocess all previous CARS data
#'
#' @param mode data source - "api"/"file"
#'
#' @return data frame
#'
#' @export

get_all_waves <- function() {

    data <- CARS::get_tidy_data_file ("2024_data.csv")
    w5_data <- CARS::get_tidy_data_file ("2023_data.csv")
    w4_data <- CARS::get_tidy_data_file ("2022_data.csv")
    w3_data <- CARS::get_tidy_data_file ("2021_data.csv")

    data <- CARS::clean_data(data, config)
    data <- CARS::derive_language_status(data)
    data$year <- 2024


  w5_data <- w5_data |>
    CARS::w5_rename_cols() |>
    CARS::w5_apply_skip_logic() |>
    CARS::w5_clean_data() |>
    CARS::w5_derive_vars()
  w5_data$year <- 2023

  w4_data <- w4_data |>
    CARS::w4_rename_cols() |>
    CARS::w4_enforce_streaming() |>
    CARS::w4_clean_departments()
  w4_data$year <- 2022

  w3_data <- w3_data |>
    CARS::w3_rename_cols() |>
    CARS::w3_enforce_streaming()
  w3_data$year <- 2021


  data <- dplyr::bind_rows(data, w5_data, w4_data, w3_data)

  return(data)
}
