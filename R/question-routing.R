
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

  conditions <- list(data$workplace %in% c("Civil service, including devolved administations", "test"),
                     data$department %in% c("Office for National Statistics", "test"),
                     data$highest_qualification != "Any other qualification",
                     data$code_freq != "Never",
                     data$other_coding_experience != "No",
                     data$prev_coding_experience != "No",
                     data$heard_of_RAP != "No")

  skipped_cols <- list(colnames(data)[which(colnames(data) == "cs_grade"):which(colnames(data) == "ONS_directorate")],
                       colnames(data)[which(colnames(data) == "ONS_directorate")],
                       colnames(data)[which(colnames(data) == "qual_1_subject"):which(colnames(data) == "qual_3_learn_code")],
                       colnames(data)[which(colnames(data) == "prac_use_open_source"):which(colnames(data) == "misc_coding")],
                       colnames(data)[which(colnames(data) == "coding_ability_change"):which(colnames(data) == "first_learned")],
                       colnames(data)[which(colnames(data) == "first_learned")],
                       colnames(data)[which(colnames(data) == "know_RAP_champ"):which(colnames(data) == "RAP_comments")])

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

enforce_skip_logic <- function(data, condition, skipped_cols) {

  row_index <- check_skip_logic(data, condition, skipped_cols)

  data[row_index, skipped_cols] <- NA

  return(data)

}
