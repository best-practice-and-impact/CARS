library(writexl)
library(magrittr)
library(dplyr)
#' #' @title Summarise CARS data by profession
#' #'
#' #' @description Generates summary tables for each profession
#' #'
#' #' @param data A cleaned CARS dataset (after using derive_vars())
#' #' @param output_file Optional file name for Excel output (default: "questions_by_profession_2023.xlsx")
#' #'
#' #' @return Writes an Excel file with one sheet per question. rows group together by profession
#' #' @export

library(dplyr)
library(writexl)

summarise_by_profession3 <- function(data, output_file = "qquestions_by_profession_2023.xlsx") {
  prof_columns <- names(data)[grepl("^prof_", names(data))]
  all_results <- list()

  # Mapping of question codes to descriptive sheet titles (no question numbers)
  sheet_titles <- list(
    code_freq = "How often do you write code to complete your work objectives?",
    knowledge = "Do you know how to program with this tool to a level suitable for your work?",
    access = "Is this tool available to use for your work?",
    where_learned = "Where did you first learn to code?",
    ability_change = "How has your coding ability changed during your current role?",
    coding_practices = "How often do you follow these coding practices in your current role?",
    doc = "How often do you write this form of documentation when programming in your current role?",
    rap_knowledge = "Have you heard anything about reproducible analytical pipelines (RAPs)?",
    rap_champ_status = "Does your department have a RAP Champion? Do you know who your RAP Champion is?",
    rap_opinions = "To what degree do you agree with the following RAP-related statements?",
    Ci = "Do you use continuous integration technologies (e.g. GitHub Actions, Jenkins, Travis, etc)?",
    dependency_management = "Do you manage dependencies using tools like requirements files, virtual environments, or containers?",
    rep_workflow = "Do you use reproducible workflow packages (e.g. drake, targets, make, pymake, etc)?",
    line_manage = "Do you line manage anyone who writes code to complete their work?",
    git_knowledge = "Do you know how to use git to version-control your work?",
    git_access = "Is git available to use in your work?"
  )

  # Questions to exclude
  exclude_questions <- c("language_status", "basic_rap_scores", "advanced_rap_scores", "rap_components")

  for (prof_col in prof_columns) {
    prof_code <- sub("prof_", "", prof_col)
    prof_data <- data %>% filter(!!sym(prof_col) == "Yes")
    if (nrow(prof_data) == 0) next

    result <- try(summarise_all(prof_data, sample = TRUE), silent = TRUE)
    if (inherits(result, "try-error")) next

    for (q in names(result)) {
      if (q %in% exclude_questions) next

      result[[q]] <- result[[q]] %>% mutate(profession = prof_code)

      if (q %in% names(all_results)) {
        all_results[[q]] <- bind_rows(all_results[[q]], result[[q]])
      } else {
        all_results[[q]] <- result[[q]]
      }
    }
  }

  # Rename sheets using the mapping
  named_results <- setNames(all_results[names(all_results) %in% names(sheet_titles)],
                            sheet_titles[names(all_results) %in% names(sheet_titles)])

  write_xlsx(named_results, output_file)
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


View(data)
# data <- CARS::get_tidy_data_file("2023_data.csv")
# data <- rename_cols(data)
# data <- apply_skip_logic(data)
# data <- clean_data(data)
# data <- derive_vars(data)

summarise_by_profession(data)
summarise_by_department(data, output_file = "questions_by_department_2023.xlsx")
