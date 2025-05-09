
#' @title Summarise CARS data by profession
#'
#' @description Generates summary tables for each profession
#'
#' @param data A cleaned CARS dataset (after using derive_vars())
#' @param output_file Optional file name for Excel output (default: "questions_by_profession_2023.xlsx")
#'
#' @return Writes an Excel file with one sheet per question. rows group together by profession
#' @export

summarise_by_profession <- function(data, output_file = "questions_by_profession_2023.xlsx") {
  prof_columns <- names(data)[grepl("^prof_", names(data))]
  all_results <- list()

  for (prof_col in prof_columns) {
    prof_code <- sub("prof_", "", prof_col)

    prof_data <- data %>%
      filter(!!sym(prof_col) == "Yes")

    if (nrow(prof_data) == 0) next

    result <- try(summarise_all(prof_data, sample = TRUE), silent = TRUE)
    if (inherits(result, "try-error")) next

    for (q in names(result)) {
      result[[q]] <- result[[q]] %>%
        mutate(profession = prof_code)

      if (q %in% names(all_results)) {
        all_results[[q]] <- bind_rows(all_results[[q]], result[[q]])
      } else {
        all_results[[q]] <- result[[q]]
      }
    }
  }

  write_xlsx(all_results, output_file)
}


#' @title Summarises CARS data by department
#'
#' @description Generates summary tables for each department
#'
#' @param data A cleaned CARS dataset (after using derive_vars())
#' @param output_file Optional file name for Excel output (default: "questions_by_department_2023.xlsx")
#'
#' @return Writes an Excel file with one sheet per question. rows group together by department
#' @export

summarise_by_department <- function(data, output_file = "questions_by_department_2023.xlsx") {
  departments <- unique(na.omit(data$department))
  all_results <- list()

  for (dept in departments) {
    dept_data <- dplyr::filter(data, department == dept)
    if (nrow(dept_data) == 0) next

    result <- try(summarise_all(dept_data, sample = TRUE), silent = TRUE)
    if (inherits(result, "try-error")) next

    for (q in names(result)) {
      result[[q]] <- result[[q]] %>%
        mutate(department = dept)

      if (q %in% names(all_results)) {
        all_results[[q]] <- bind_rows(all_results[[q]], result[[q]])
      } else {
        all_results[[q]] <- result[[q]]
      }
    }
  }

  write_xlsx(all_results, output_file)
}

data <- CARS::get_tidy_data_file("2023_data.csv") %>%
  CARS::rename_cols() %>%
  CARS::apply_skip_logic() %>%
  CARS::clean_data() %>%
  CARS::derive_vars()


summarise_by_profession(data)
summarise_by_department(data, output_file = "questions_by_department_2023.xlsx")
