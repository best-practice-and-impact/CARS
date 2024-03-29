
#' @title Apply skip logic
#'
#' @description Iteratively applies enforce_skip_logic to the necessary fields in the data.
#'
#' @param data data.frame
#'
#' @return cleaned data.frame
#'
#' @export

apply_skip_logic <- function(data) {

  conditions <- list(data$workplace == "NHS or local healthcare service",
                     !data$workplace %in% c("Civil service, including devolved administrations", "NHS or local healthcare service") & !is.na(data$workplace) & data$workplace != "test",
                     data$department != "Office for National Statistics" & !is.na(data$department) & data$department != "test",
                     !is.na(data$ONS_directorate) & data$ONS_directorate != "test",
                     data$pay_band == "Local Authority or NJC",
                     data$pay_band == "Other / Not sure",
                     !is.na(data$NHS_band) & data$NHS_band != "test",
                     data$primary_work_country == "Scotland",
                     data$primary_work_country == "Wales",
                     data$primary_work_country == "Northern Ireland",
                     !is.na(data$England_NHS_organisation) & data$England_NHS_organisation != "test",
                     !is.na(data$Scotland_NHS_organisation) & data$Scotland_NHS_organisation != "test",
                     !is.na(data$Wales_NHS_organisation) & data$Wales_NHS_organisation != "test",
                     data$highest_qualification == "Any other qualification",
                     data$code_freq == "Never",
                     data$other_coding_experience == "No",
                     data$heard_of_RAP == "No",
                     data$have_RAP_champ != "Yes" & !is.na(data$have_RAP_champ) & data$have_RAP_champ != "test")

  skipped_cols <- list(colnames(dplyr::select(data, "CS_grade":"ONS_directorate")),
                       colnames(dplyr::select(data, "CS_grade":"Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "ONS_directorate":"Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "pay_band":"Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "NHS_band")),
                       colnames(dplyr::select(data, "NHS_band":"NJC_grade")),
                       colnames(dplyr::select(data, "NJC_grade")),
                       colnames(dplyr::select(data, "England_NHS_organisation")),
                       colnames(dplyr::select(data, "England_NHS_organisation":"Scotland_NHS_organisation")),
                       colnames(dplyr::select(data, "England_NHS_organisation":"Wales_NHS_organisation")),
                       colnames(dplyr::select(data, "Scotland_NHS_organisation":"Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "Wales_NHS_organisation":"Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "Northern_Ireland_NHS_organisation")),
                       colnames(dplyr::select(data, "qual_1_subject":"qual_3_learn_code")),
                       colnames(dplyr::select(data, "other_coding_experience":"reproducible_workflow")),
                       colnames(dplyr::select(data, "first_learned":"coding_ability_change")),
                       colnames(dplyr::select(data, "have_RAP_champ":"RAP_comments")),
                       colnames(dplyr::select(data, "know_RAP_champ")))

  for(i in 1:length(conditions)){
    data <- enforce_skip_logic(data, conditions[[i]], skipped_cols[[i]])
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

check_skip_logic <- function(data, condition, skipped_cols) {

  condition_met <- condition & !is.na(data[skipped_cols])

  row_failed <- as.logical(rowSums(condition_met))

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

enforce_skip_logic <- function(data, condition, skipped_cols) {

  row_index <- check_skip_logic(data, condition, skipped_cols)

  data[row_index, skipped_cols] <- NA

  return(data)

}
