

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
