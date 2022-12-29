library(magrittr)

data <- CARS::get_tidy_data() %>% CARS::rename_cols() %>% CARS::apply_skip_logic() %>% CARS::derive_vars()

render_site <- function(data, path = "quarto/main") {
  tables <- CARS::summarise_all(data, all_tables = TRUE)

  dir.create(paste0(path, "/temp"))
  save(tables, file = paste0(path, "/temp/summary_tables.Rda"))

  quarto::quarto_render(input = path, as_job = FALSE)

  unlink(paste0(path, "/temp"), recursive = TRUE)
}


