
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
  if (ncol(data) != 109) {
    stop("Unexpected input: incorrect number of columns. Please use the 2024 CARS dataset.")
  }

  data <- data[!colnames(data) %in% c("UserID", "Unique.ID", "Name", "Email", "IP.Address", "Started", "Ended")]
  colnames(data)[c(1:ncol(data))] <- c(
    "ID",
    "tracking_link",
    "workplace",
    "cs_grade",
    "department",
    "department_other",
    names(config[["professions"]][["cols"]]),
    "ons_directorate",
    "pay_band",
    "nhs_band",
    "njc_grade",
    "nhs_country",
    "nhs_england",
    "nhs_scotland",
    "nhs_wales",
    "nhs_ni",
    "coding_exp",
    "team",
    "management",
    "code_freq",
    "code_leisure",
    "first_learned",
    "coding_years",
    "ability_change",
    names(config[["coding_tools_access"]][["cols"]]),
    names(config[["coding_tools_knowledge"]][["cols"]]),
    names(config[["coding_practices"]][["cols"]]),
    names(config[["working_practices"]][["cols"]]),
    names(config[["doc"]][["cols"]]),
    "ai",
    names(config[["ai_tools"]][["cols"]]),
    names(config[["ai_use"]][["cols"]]),
    "ai_trust",
    "heard_of_rap",
    names(config[["rap_opinions"]][["cols"]]),
    "qs_aware",
    "qs_comply",
    "qq_aware",
    "comments",
    "future_surveys"
  )



  return(data)
}

#' @title Clean data
#'
#' @description Recategorise department, workplace and first_learned data
#'
#' @param data cleaned CARS dataset
#'
#' @return CARS dataset
#' @export

clean_data <- function(data){

 data %>%
   clean_departments() %>%
   clean_workplace() %>%
   clean_first_learned()

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

#' @title Clean first learned data
#'
#' @description reclassify 'other' free text responses into self-taught based on common terms used
#'
#' @param data cleaned CARS dataset
#'
#' @return CARS dataset
#' @export

clean_first_learned <- function(data) {

  matches <- c("self",
               "hobby",
               "personal",
               "independ",
               "home",
               "for fun",
               "free time",
               "spare time",
               "childhood")

  data$first_learned[stringr::str_detect(tolower(data$first_learned), stringr::str_c(matches, collapse = "|"))] <- "Self-taught"

  return(data)

}
