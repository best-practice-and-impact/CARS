
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
  data <- dplyr::mutate(data, across(c(4:27), ~ dplyr::case_when(!workplace %in% c("Civil service, including devolved administrations", "NHS or local healthcare service") & !is.na(data$workplace) ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(19:27, 98:100), ~ dplyr::case_when(!department %in% c("Office for National Statistics", "test") & !is.na(department) ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(20:27), ~ dplyr::case_when(!is.na(ons_directorate) & ons_directorate != "test" ~ NA, .default = .)))

  data[, "nhs_band"]  <- dplyr::case_when(data$pay_band %in% c("Local Authority or NJC", "Other / Not sure", "test") ~ NA, .default = data$nhs_band)
  data[, "njc_grade"] <- dplyr::case_when(data$pay_band %in% c("NHS", "Other / Not sure", "test") ~ NA, .default = data$njc_grade)
  data[, "nhs_england"] <- dplyr::case_when(data$nhs_country %in% c("Scotland", "Wales", "Northern Ireland", "test") ~ NA, .default = data$nhs_england)
  data[, "nhs_scotland"] <- dplyr::case_when(data$nhs_country %in% c("England", "Wales", "Northern Ireland", "test") ~ NA, .default = data$nhs_scotland)
  data[, "nhs_wales"] <- dplyr::case_when(data$nhs_country %in% c("England", "Scotland", "Northern Ireland", "test") ~ NA, .default = data$nhs_wales)
  data[, "nhs_ni"] <- dplyr::case_when(data$nhs_country %in% c("England", "Scotland", "Wales", "test") ~ NA, .default = data$nhs_ni)

  data <- dplyr::mutate(data, across(c(31:91), ~ dplyr::case_when(coding_exp == "No" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(56:91), ~ dplyr::case_when(code_freq == "Never" ~ NA, .default = .)))
  data[, "ability_change"] <- dplyr::case_when(data$first_learned == "Current role" ~ NA, .default = data$ability_change)
  data <- dplyr::mutate(data, across(c(76:91), ~ dplyr::case_when(ai == "No" ~ NA, .default = .)))
  data <- dplyr::mutate(data, across(c(93:97), ~ dplyr::case_when(heard_of_rap == "No" ~ NA, .default = .)))

  return(data)

}

