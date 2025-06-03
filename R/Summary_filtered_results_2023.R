library(writexl)
library(magrittr)
library(dplyr)
library(aftables)
library(openxlsx)

#' @title Summarise CARS data by profession
#'
#' @description Generates summary tables for each profession
#'
#' @param data A cleaned CARS dataset (after using derive_vars())
#' @param output_file Optional file name for Excel output (default: "Summary_by_profession_2023.xlsx")
#'
#' @return Writes an Excel file with one sheet per question. Rows group together by profession
#' @export


summarise_by_profession <- function(data, output_file = "docs/Summary_by_profession_2023.xlsx") {
  prof_columns <- names(data)[grepl("^prof_", names(data))]
  all_results <- list()

  question_labels <- c(
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

  exclude_questions <- c("language_status", "basic_rap_scores", "advanced_rap_scores", "rap_components")

  for (prof_col in prof_columns) {
    prof_code <- sub("prof_", "", prof_col)
    prof_data <- data %>% dplyr::filter(!!sym(prof_col) == "Yes")
    if (nrow(prof_data) == 0) next

    result <- try(summarise_all(prof_data, sample = TRUE), silent = TRUE)
    if (inherits(result, "try-error")) next
    for (q in names(result)) {
      if (q %in% exclude_questions) next

      result[[q]] <- result[[q]] %>% dplyr::mutate(profession = prof_code)
      if (q %in% names(all_results)) {
        all_results[[q]] <- dplyr::bind_rows(all_results[[q]], result[[q]])
      } else {
        all_results[[q]] <- result[[q]]
      }
    }
  }

  valid_questions <- names(all_results)[names(all_results) %in% names(question_labels)]

  contents <- data.frame(
    "Sheet name" = valid_questions,
    "Sheet title" = unname(question_labels[valid_questions]),
    check.names = FALSE
  )

  pad_table <- function(tbl) {
    if (nrow(tbl) < 4) {
      tbl <- rbind(tbl, matrix(NA, nrow = 4 - nrow(tbl), ncol = ncol(tbl)))
    }
    current_cols <- ncol(tbl)
    if (current_cols < 7) {
      for (i in 1:(7 - current_cols)) {
        tbl[[paste0("pad_", i)]] <- NA
      }
    }
    return(tbl)
  }

  tables <- c(list(contents), all_results[valid_questions])
  table_names <- c("contents", valid_questions)

  tables <- Map(function(tbl, name) {
    if (!"source" %in% names(tbl)) {
      tbl$source <- "Source: CARS 2023 dataset"
    }
    if (name %in% names(question_labels)) {
      colnames(tbl)[1] <- question_labels[[name]]
    }
    pad_table(tbl)
  }, tables, table_names)

  cover_list <- list(
    "Title" = "Coding in Analysis and Research Survey",
    "Subtitle" = "2023",
    "Date" = format(Sys.Date(), "%d %B %Y"),
    "Information" = c(
      "The results presented here are from a self-selecting sample of government analysts.",
      "Data will only be provided for professions with over 20 responses.",
      "Data are presented as frequency tables, with count and sample size.",
      "Sample size may vary due to question streaming.",
      "Please see the CARS website for the most recent information."
    ),
    "Contact" = "Any questions, contact: ASAP@ons.gov.uk"
  )

  aftable_obj <- create_aftable(
    tab_titles = c("cover", table_names),
    sheet_titles = c("Cover", "Contents", unname(question_labels[valid_questions])),
    sheet_types = c("cover", "contents", rep("tables", length(valid_questions))),
    tables = c(list(cover_list), tables)
  )

  wb <- generate_workbook(aftable_obj)
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
}



#' @title Summarises CARS data by department
#'
#' @description Generates summary tables for each department
#'
#' @param data A cleaned CARS dataset (after using derive_vars())
#' @param output_file Optional file name for Excel output (default: "Summary_by_department_2023.xlsx")
#'
#' @return Writes an Excel file with one sheet per question. Rows group together by department
#' @export
summarise_by_department <- function(data, output_file = "docs/Summary_by_department_2023.xlsx") {
  departments <- unique(na.omit(data$department))
  all_results <- list()

  question_labels <- c(
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

  exclude_questions <- c("language_status", "basic_rap_scores", "advanced_rap_scores", "rap_components")

  for (dept in departments) {
    dept_data <- dplyr::filter(data, department == dept)
    if (nrow(dept_data) == 0) next

    result <- try(summarise_all(dept_data, sample = TRUE), silent = TRUE)
    if (inherits(result, "try-error")) next

    for (q in names(result)) {
      if (q %in% exclude_questions) next

      result[[q]] <- result[[q]] %>% dplyr::mutate(department = dept)

      if (q %in% names(all_results)) {
        all_results[[q]] <- dplyr::bind_rows(all_results[[q]], result[[q]])
      } else {
        all_results[[q]] <- result[[q]]
      }
    }
  }

  valid_questions <- names(all_results)[names(all_results) %in% names(question_labels)]

  contents <- data.frame(
    "Sheet name" = valid_questions,
    "Sheet title" = unname(question_labels[valid_questions]),
    check.names = FALSE
  )
  pad_table <- function(tbl) {
    # Ensure at least 4 rows
    if (nrow(tbl) < 4) {
      tbl <- rbind(tbl, matrix(NA, nrow = 4 - nrow(tbl), ncol = ncol(tbl)))
    }

    # Pad to 7 columns without overwriting existing names
    current_cols <- ncol(tbl)
    if (current_cols < 7) {
      for (i in 1:(7 - current_cols)) {
        tbl[[paste0("pad_", i)]] <- NA
      }
    }

    return(tbl)
  }



  tables <- c(list(contents), all_results[valid_questions])
  table_names <- c("contents", valid_questions)

  tables <- Map(function(tbl, name) {
    if (!"source" %in% names(tbl)) {
      tbl$source <- "Source: CARS 2023 dataset"
    }
    if (name %in% names(question_labels)) {
      colnames(tbl)[1] <- question_labels[[name]]
    }
    pad_table(tbl)
  }, tables, table_names)

  # Create the cover as a list
  cover_list <- list(
    "Title" = "Coding in Analysis and Research Survey",
    "Subtitle" = "2023",
    "Date" = format(Sys.Date(), "%d %B %Y"),
    "Information" = c(
      "The results presented here are from a self-selecting sample of government analysts.",
      "Data will only be provided for departments with over 20 responses.",
      "Data are presented as frequency tables, with count and sample size.",
      "Sample size may vary due to question streaming.",
      "Please see the CARS website for the most recent information."
    ),
    "Contact" = "Any questions, contact: ASAP@ons.gov.uk"
  )

  # Create aftable with the cover list
  aftable_obj <- create_aftable(
    tab_titles = c("cover", table_names),
    sheet_titles = c("Cover", "Contents", unname(question_labels[valid_questions])),
    sheet_types = c("cover", "contents", rep("tables", length(valid_questions))),
    tables = c(list(cover_list), tables)
  )

  wb <- generate_workbook(aftable_obj)
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
}



data <- CARS::get_tidy_data_file("2023_data.csv") %>%
  CARS::rename_cols() %>%
  CARS::apply_skip_logic() %>%
  CARS::clean_data() %>%
  CARS::derive_vars()



summarise_by_profession(data)
summarise_by_department(data, output_file = "Summary_by_department_2023.xlsx")
