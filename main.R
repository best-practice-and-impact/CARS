library(magrittr)

data <- CARS::get_tidy_data_file("2024_data.csv")
config <- yaml::read_yaml("config.yml")

data<- CARS::clean_data(data, config)
data <- CARS::derive_vars(data)

CARS::create_filtered_pages(data, type = "departments")
CARS::create_filtered_pages(data, type = "professions")
CARS::render_site()
