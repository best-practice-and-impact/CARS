library(magrittr)

data <- CARS::get_tidy_data() %>% CARS::rename_cols() %>% CARS::apply_skip_logic() %>% CARS::derive_vars()

CARS::render_site(data)
