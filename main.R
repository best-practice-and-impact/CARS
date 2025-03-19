library(magrittr)
library(CARS)

data <- CARS::get_tidy_data_file("2024_data.csv")
config <- yaml::read_yaml("config.yml")

data <- clean_data(data, config)

CARS::create_filtered_pages(data, type = "departments")
CARS::create_filtered_pages(data, type = "professions")
#CARS::render_site()
