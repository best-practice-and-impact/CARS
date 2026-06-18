
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
  data <- dplyr::mutate(data, across(c(20:22), ~ dplyr::case_when(workplace == "Civil service, including devolved administrations"  ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(4:22), ~ dplyr::case_when(!workplace %in% c("Civil service, including devolved administrations", "NHS or local healthcare service") & !is.na(data$workplace) ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(127:129), ~ dplyr::case_when(workplace != "Civil service, including devolved administrations" | department != "Office for National Statistics" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(20:22), ~ dplyr::case_when(!is.na(ons_directorate) & ons_directorate != "test" ~ NA, .default = .)))

  data[, "nhs_band"]  <- dplyr::case_when(data$pay_band %in% c("Local Authority or NJC", "Other / Not sure", "test") & !is.na(data$pay_band)  ~ NA, .default = data$nhs_band)
  data[, "njc_grade"] <- dplyr::case_when(data$pay_band %in% c("NHS", "Other / Not sure", "test") & !is.na(data$pay_band)  ~ NA, .default = data$njc_grade)

  data <- dplyr::mutate(data, across(c(25:35), ~ dplyr::case_when(coding_exp == "Yes" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(25:26, 125:126), ~ dplyr::case_when(coding_exp == "No" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(78:105), ~ dplyr::case_when(code_freq == "Never" ~ NA, .default = .)))
  # data <- dplyr::mutate(data, across(c(26:126), ~ dplyr::case_when(coding_learn_pref %in% c("Yes", "No", "Not sure / not applicable") | department != "Office for National Statistics" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(79:93), ~ dplyr::case_when(ai == "No" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(126), ~ dplyr::case_when(packages != "Yes" ~ NA, .default = .)))

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
  data$department[data$department_other %in% c("Department of Finance", "dept of finance")] <- "Northern Ireland Executive"
  data$department[data$department_other %in% c("National Records of Scotland", "NRS", "National Records of Scotland (Census team) ")] <- "National Records of Scotland"
  data$department[data$department_other %in% c("The Health and Safety Executive", "Health and Safety Executive ", "Health and Safety Executive", "Health & Safety Executive")] <- "Health and Safety Executive"
  data$department[data$department_other == "UKHSA"] <- "UK Health Security Agency"
  data$department[data$department_other == "Fast Stream Cabinet Office"] <- "Cabinet Office (excl. agencies)"

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
  if (ncol(data) != 140) {
    stop("Unexpected input: incorrect number of columns. Please use the 2026 CARS dataset.")
  }

  normalise_spaces <- function(x) {
    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    x <- trimws(x)
    return(x)
  }

  flat <- unlist(config$rename_dict, recursive = TRUE, use.names = TRUE)
  old_keys <- sub("^[^.]+\\.", "", names(flat))
  rename_dict <- setNames(unname(flat), old_keys)

  names(data) <- normalise_spaces(names(data))
  names(rename_dict) <- normalise_spaces(names(rename_dict))

  data <- data %>%
    rename_with(~ dplyr::coalesce(unname(rename_dict[.x]), .x))

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
