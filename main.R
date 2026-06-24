library(magrittr)

data <- CARS::get_tidy_data_file("2026_data.csv")
config <- yaml::read_yaml("config.yml")

data <- CARS::clean_data(data, config)
data <- CARS::derive_vars(data)
data$year <- 2026
file_path <- file.path(Sys.getenv("CARS_DATA_DIR"), "2026_cleaned_data.rds")
saveRDS(data, file_path)

CARS::create_filtered_pages(data, type = "departments")
CARS::create_filtered_pages(data, type = "professions")
CARS::render_site()
