library(magrittr)

data <- CARS::get_tidy_data_file("2023_data.csv") %>%
  CARS::rename_cols() %>%
  CARS::apply_skip_logic() %>%
  CARS::clean_departments() %>%
  CARS::derive_vars()

CARS::create_filtered_pages(data, type = "departments")
CARS::create_filtered_pages(type = "professions")
CARS::render_site()
