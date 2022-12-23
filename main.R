library(magrittr)

data <- CARS::get_tidy_data() %>% CARS::rename_cols() %>% CARS::apply_skip_logic() %>% CARS::derive_vars()

tables <- CARS::summarise_all(data, all_tables = TRUE)
