#' @title Create and cache all CARS data
#'
#' @description Process and save cleaned CARS data as separate per-year cache files
#'
#' @param cache_dir Directory to save yearly cache files
#' @param prefix Filename prefix for yearly cache files
#' @param ... Named configuration parameters (e.g., config, w6_config)
#'
#' @return Named list of yearly data frames (invisibly)
#'
#' @keywords internal
create_all_waves_cache <- function(
    cache_dir = Sys.getenv("CARS_DATA_DIR"),
    prefix = "cars_cleaned",
    ...
) {
  configs <- list(...)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  data <- CARS::get_tidy_data_file("2026_data.csv")
  w6_data <- CARS::get_tidy_data_file("2024_data.csv")
  w5_data <- CARS::get_tidy_data_file("2023_data.csv")
  w4_data <- CARS::get_tidy_data_file("2022_data.csv")

  data <- CARS::clean_data(data, configs$config)
  data <- CARS::derive_language_status(data)
  data$year <- 2026

  w6_data <- w6_data |>
    CARS::w6_clean_data(configs$w6_config) |>
    CARS::w6_derive_vars()
  w6_data$year <- 2024

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

  yearly_data <- list(
    "2026" = data,
    "2024" = w6_data,
    "2023" = w5_data,
    "2022" = w4_data
  )

  for (yr in names(yearly_data)) {
    out_file <- file.path(cache_dir, paste0(prefix, "_", yr, ".rds"))
    saveRDS(yearly_data[[yr]], out_file)
  }

  invisible(yearly_data)
}

config <- yaml::read_yaml("config.yml")
w6_config <- yaml::read_yaml("w6_config.yml")
create_all_waves_cache(config = config, w6_config = w6_config)
