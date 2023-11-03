
#' @title Rename columns
#'
#' @description Renames columns and removes unnecessary columns
#'
#' @param data tidy CARS dataset
#'
#' @return data.frame
#'
#' @export

w4_rename_cols <- function(data) {
  if (ncol(data) != 112) {
    stop("Unexpected input: incorrect number of columns. Please use the 2022 CARS dataset.")
  }

  colnames(data)[c(1, 7:ncol(data))] <- c(
    "ID",
    "started",
    "ended",
    "tracking_link",
    "workplace",
    "CS_grade",
    "department",
    "other_department_name",
    "prof_DS",
    "prof_DDAT",
    "prof_GAD",
    "prof_GES",
    "prof_geog",
    "prof_GORS",
    "prof_GSR",
    "prof_GSG",
    "prof_CS_none",
    "prof_CS_other",
    "ONS_directorate",
    "highest_qualification",
    "qual_1_subject",
    "qual_1_level",
    "qual_1_learn_code",
    "qual_2_subject",
    "qual_2_level",
    "qual_2_learn_code",
    "qual_3_subject",
    "qual_3_level",
    "qual_3_learn_code",
    "code_freq",
    "management",
    "ops_analysis",
    "ops_cleaning",
    "ops_linking",
    "ops_transfer_migration",
    "ops_vis",
    "ops_machine_learning",
    "ops_modelling",
    "ops_QA",
    "ops_other",
    "ops_other_name",
    "knowledge_R",
    "access_R",
    "knowledge_SQL",
    "access_SQL",
    "knowledge_SAS",
    "access_SAS",
    "knowledge_VBA",
    "access_VBA",
    "knowledge_python",
    "access_python",
    "knowledge_SPSS",
    "access_SPSS",
    "knowledge_stata",
    "access_stata",
    "knowledge_JS",
    "access_JS",
    "knowledge_java",
    "access_java",
    "knowledge_C",
    "access_C",
    "knowledge_matlab",
    "access_matlab",
    "knowledge_access_other",
    "knowledge_git",
    "access_git",
    "other_coding_experience",
    "coding_ability_change",
    "prev_coding_experience",
    "first_learned",
    "heard_of_RAP",
    "know_RAP_champ",
    "strategy_knowledge",
    "RAP_confident",
    "RAP_supported",
    "RAP_resources",
    "RAP_components",
    "RAP_important",
    "RAP_implementing",
    "RAP_planning",
    "RAP_comments",
    "prac_use_open_source",
    "prac_open_source_own",
    "prac_version_control",
    "prac_review",
    "prac_functions",
    "prac_unit_test",
    "prac_package",
    "prac_dir_structure",
    "prac_style",
    "prac_automated_QA",
    "prac_AQUA_book",
    "doc_comments",
    "doc_functions",
    "doc_readme",
    "doc_desk_notes",
    "doc_registers",
    "doc_AQA_logs",
    "doc_flow_charts",
    "doc_other",
    "CI",
    "dep_management",
    "reproducible_workflow",
    "misc_coding",
    "misc_support",
    "misc_additional_data",
    "misc_other"
  )

  data <- data[!colnames(data) %in% c("UserNo", "Name", "Email", "IP.Address", "Unique.ID")]

  return(data)
}

#' @title Clean department data
#'
#' @description add NHS to department list and merge departments where needed.
#'
#' @param data cleaned CARS dataset
#'
#' @return CARS dataset
#' @export

clean_departments <- function(data) {

  data$department[grepl("forest research", tolower(data$other_department_name))] <- "Forestry Commission"

  data$department[data$workplace == "NHS"] <- "NHS"

  defra_orgs <- c(
    "Department for Environment, Food and Rural Affairs (excl. agencies)",
    "Forestry Commission",
    "Animal and Plant Health Agency",
    "Centre for Environment, Fisheries and Aquaculture Science",
    "Rural Payments Agency",
    "Environment Agency",
    "Marine Management Organisation",
    "Natural England"
  )

  data$defra <- data$department %in% defra_orgs

  return(data)

}



#' @title Apply skip logic
#'
#' @description Iteratively applies enforce_skip_logic to the necessary fields in the data.
#'
#' @param data data.frame
#'
#' @return cleaned data.frame
#'
#' @export

w4_apply_skip_logic <- function(data) {

  conditions <- list(data$workplace %in% c("Civil service, including devolved administations", "test"),
                     data$department %in% c("Office for National Statistics", "test"),
                     data$highest_qualification != "Any other qualification",
                     data$code_freq != "Never",
                     data$other_coding_experience != "No",
                     data$prev_coding_experience != "No",
                     data$heard_of_RAP != "No")

  skipped_cols <- list(colnames(data)[which(colnames(data) == "CS_grade"):which(colnames(data) == "ONS_directorate")],
                       colnames(data)[which(colnames(data) == "ONS_directorate")],
                       colnames(data)[which(colnames(data) == "qual_1_subject"):which(colnames(data) == "qual_3_learn_code")],
                       colnames(data)[which(colnames(data) == "prac_use_open_source"):which(colnames(data) == "misc_coding")],
                       colnames(data)[which(colnames(data) == "coding_ability_change"):which(colnames(data) == "first_learned")],
                       colnames(data)[which(colnames(data) == "first_learned")],
                       colnames(data)[which(colnames(data) == "know_RAP_champ"):which(colnames(data) == "RAP_comments")])

  for(i in 1:length(conditions)){
    data <- w4_enforce_skip_logic(data, conditions[[i]], skipped_cols[[i]])
  }

  return(data)

}


#' @title Check skip logic
#'
#' @description Checks whether the skip logic was followed correctly. Backtracking while filling the survey can result in inconsistent response sets.
#' This check returns row numbers where questions which should have been skipped contain anything other than NA.
#'
#' @param data data.frame
#' @param condition logical vector. Example: data$row == "skip response"
#' @param skipped_cols character. questions that should have been skipped if condition != TRUE
#'
#' @return list of rows failing the check
#'
#' @export

w4_check_skip_logic <- function(data, condition, skipped_cols) {

  condition_failed <- !condition & !is.na(data[skipped_cols])

  row_failed <- as.logical(rowSums(condition_failed))


  return(
    which(row_failed)
  )

}

#' @title enforce skip logic
#'
#' @description Replaces values in rows with NAs where check_skip_logic has identified backtracking.
#'
#' @param data data.frame
#' @param condition logical vector. Example: data$row == "skip response"
#' @param skipped_cols character. questions that should have been skipped if condition != TRUE
#'
#' @return data.frame with rows failing the check replaced with NAs
#'
#' @export

w4_enforce_skip_logic <- function(data, condition, skipped_cols) {

  row_index <- w4_check_skip_logic(data, condition, skipped_cols)

  data[row_index, skipped_cols] <- NA

  return(data)

}
