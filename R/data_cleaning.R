
#' @title Apply skip logic
#'
#' @description Applies skip logic to dataset, returning NA for downstream questions to adhere to survey streaming.
#'
#' @param data data.frame
#'
#' @return cleaned data.frame
#'
#' @export

apply_skip_logic <- function(data) {

  data <- dplyr::mutate(data, across(c(4:19), ~ dplyr::case_when(workplace == "NHS or local healthcare service" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(20:27), ~ dplyr::case_when(workplace == "Civil service, including devolved administrations"  ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(4:27), ~ dplyr::case_when(!workplace %in% c("Civil service, including devolved administrations", "NHS or local healthcare service") & !is.na(data$workplace) ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(19, 98:100), ~ dplyr::case_when(workplace != "Civil service, including devolved administrations" | department != "Office for National Statistics" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(20:27), ~ dplyr::case_when(!is.na(ons_directorate) & ons_directorate != "test" ~ NA, .default = .)))

  data[, "nhs_band"]  <- dplyr::case_when(data$pay_band %in% c("Local Authority or NJC", "Other / Not sure", "test") & !is.na(data$pay_band)  ~ NA, .default = data$nhs_band)
  data[, "njc_grade"] <- dplyr::case_when(data$pay_band %in% c("NHS", "Other / Not sure", "test") & !is.na(data$pay_band)  ~ NA, .default = data$njc_grade)
  data[, "nhs_england"] <- dplyr::case_when(data$nhs_country %in% c("Scotland", "Wales", "Northern Ireland", "test") ~ NA, .default = data$nhs_england)
  data[, "nhs_scotland"] <- dplyr::case_when(data$nhs_country %in% c("England", "Wales", "Northern Ireland", "test") ~ NA, .default = data$nhs_scotland)
  data[, "nhs_wales"] <- dplyr::case_when(data$nhs_country %in% c("England", "Scotland", "Northern Ireland", "test") ~ NA, .default = data$nhs_wales)
  data[, "nhs_ni"] <- dplyr::case_when(data$nhs_country %in% c("England", "Scotland", "Wales", "test") & !is.na(data$nhs_country) ~ NA, .default = data$nhs_ni)

  data <- dplyr::mutate(data, across(c(31:91), ~ dplyr::case_when(coding_exp == "No" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(56:91), ~ dplyr::case_when(code_freq == "Never" ~ NA, .default = .)))
  data[, "ability_change"] <- dplyr::case_when(data$first_learned == "Current role" ~ NA, .default = data$ability_change)
  data <- dplyr::mutate(data, across(c(76:91), ~ dplyr::case_when(ai == "No" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(93:97), ~ dplyr::case_when(heard_of_rap == "No" ~ NA, .default = .)))

  return(data)

}


#' @title Clean data
#'
#' @description Rename columns, enforce streaming
#'
#' @param data cleaned CARS dataset
#' @param config CARS config
#'
#' @return CARS dataset
#' @export

clean_data <- function(data, config){

 data <- rename_cols(data, config)
 data <- data |>
   apply_skip_logic() |>
   clean_departments() |>
   clean_first_learned() |>
   clean_quality_qs()

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

  data$department[data$workplace == "NHS or local healthcare service"] <- "NHS"
  data$department[data$department_other == "Office for National Statistics"] <- "Office for National Statistics"
  data$department[data$department_other == "Natural England"] <- "Natural England"
  data$department[data$department_other %in% c("Welsh Revenue Authority", "WRA")] <- "Welsh Government"
  data$department[data$department_other == "Environment Agency"] <- "Environment Agency"
  data$department[data$workplace %in% c("Environment Agency", "EA")] <- "Environment Agency"
  data$department[data$department_other == "Home Office "] <- "Home Office"
  data$department[data$department_other == "Centre for Environment Fisheries and Aquaculture Science"] <- "Centre for Environment, Fisheries and Aquaculture Science"
  data$department[data$department_other == "Department of Finance"] <- "Northern Ireland Executive"
  data$department[data$department_other == "National Records of Scotland"] <- "National Records of Scotland"
  data$department[data$department_other %in% c("Health and Safety Executive ", "Health and Safety Executive")] <- "Health and Safety Executive"
  data$department[data$department_other == "UKHSA"] <- "UK Health Security Agency"

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

#' @title Clean first_learned data
#'
#' @description categorise 'other' responses.
#'
#' @param data cleaned CARS dataset
#'
#' @return CARS dataset
#' @export

clean_first_learned <- function(data) {

  he_match <- c("PhD",
               "MSc",
               "University")

  data$first_learned[stringr::str_detect(tolower(data$first_learned), stringr::str_c(he_match, collapse = "|"))] <- "Higher Education"

  se_match <- c("College",
                "AS level")

  data$first_learned[stringr::str_detect(tolower(data$first_learned), stringr::str_c(he_match, collapse = "|"))] <- "Primary/secondary education"


  set_responses <- c(
  "Higher Education",
  "Primary/secondary education",
  "Self-taught",
  "Previous public sector employment",
  "Current role",
  "Previous private sector employment"
  )

  data$first_learned[!(data$first_learned %in% set_responses) & !is.na(data$first_learned)] <- "Other"

  return(data)

}

#' @title Rename columns
#'
#' @description Renames columns and removes unnecessary columns
#'
#' @param data tidy CARS dataset
#' @param config CARS config
#'
#' @return data.frame
#'
#' @export

rename_cols <- function(data, config) {
  if (ncol(data) != 109) {
    stop("Unexpected input: incorrect number of columns. Please use the 2024 CARS dataset.")
  }

  colnames(data)[c(1:ncol(data))] <- c(
    "UserID",
    "ID",
    "Name",
    "Email",
    "IP.Address",
    "Unique.ID",
    "Started",
    "Ended",
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

  data <- data[!colnames(data) %in% c("UserID", "Unique.ID", "Name", "Email", "IP.Address", "Started", "Ended")]

  return(data)
}

#' Clean quality question columns to remove whitespace
#'
#' @param data CARS 2024 dataset
#'
#' @return cleaned quality question columns

clean_quality_qs <- function(data){

  cols <- c("qs_aware", "qs_comply", "qq_aware")
  data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(cols), \(x) gsub("\u00a0", "", x)))
  data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(cols), trimws))


}
