
#' @title Rename columns
#'
#' @description Renames columns and removes unnecessary columns
#'
#' @param data tidy CARS dataset
#'
#' @return data.frame
#'
#' @export

rename_cols <- function(data) {
  if (ncol(data) != 112) {
    stop("Unexpected input: incorrect number of columns. Please use the 2023 CARS dataset.")
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
    "prof_DE",
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
    "pay_band",
    "NHS_band",
    "NJC_grade",
    "primary_work_country",
    "England_NHS_organisation",
    "Scotland_NHS_organisation",
    "Wales_NHS_organisation",
    "Northern_Ireland_NHS_organisation",
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
    "access_matlab",
    "access_python",
    "access_R",
    "access_SAS",
    "access_SPSS",
    "access_SQL",
    "access_stata",
    "access_VBA",
    "access_open_source_other",
    "access_licensed_other",
    "access_other_specified",
    "knowledge_matlab",
    "knowledge_python",
    "knowledge_R",
    "knowledge_SAS",
    "knowledge_SPSS",
    "knowledge_SQL",
    "knowledge_stata",
    "knowledge_VBA",
    "knowledge_licensed_other",
    "knowledge_open_source_other",
    "knowledge_other_specified",
    "knowledge_git",
    "access_git",
    "other_coding_experience",
    "first_learned",
    "coding_ability_change",
    "heard_of_RAP",
    "have_RAP_champ",
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
    "prac_other_automated",
    "prac_package",
    "prac_dir_structure",
    "prac_style",
    "prac_automated_QA",
    "prac_development_QA",
    "prac_proportionate_QA",
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

  data$department[data$department == "Foreign, Commonwealth & Development Office (excl. agencies)"] <- "Foreign, Commonwealth and Development Office (excl. agencies)"

  data$department[data$workplace == "NHS"] <- "NHS"

  data$department[data$other_department_name == "Office for National Statistics"] <- "Office for National Statistics"

  data$department[data$other_department_name == "Data Science Campus"] <- "Office for National Statistics"

  data$department[data$other_department_name == "Welsh Revenue Authority"] <- "Welsh Government"

  data$department[data$other_department_name == "Equality Hub, Cabinet Office"] <- "Cabinet Office (excl. agencies)"

  data$department[data$other_department_name == "Natural England"] <- "Natural England"

  data$department[data$other_department_name == "Department for Communities"] <- "Northern Ireland Executive"

  data$department[data$other_department_name == "Department of Education Northern Ireland"] <- "Northern Ireland Executive"

  defra_orgs <- c(
    "Department for Environment, Food and Rural Affairs (excl. agencies)",
    "Forestry Commission",
    "Forest Research",
    "Forestry England",
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

#' @title Clean workplace data
#'
#' @description reclassify 'other' text responses into CS/NHS
#'
#' @param data cleaned CARS dataset
#'
#' @return CARS dataset
#' @export

clean_workplace <- function(data) {

  data$workplace[data$workplace == "MOD"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "HMRC"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "The Pensions Regulator"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "Scottish Funding Council"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "Office for Students"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "Office for students"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "OfS"] <- "Civil service, including devolved administrations"

  data$workplace[data$workplace == "Dstl"] <- "Civil service, including devolved administrations"

  return(data)

}
