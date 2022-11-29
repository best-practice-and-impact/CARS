

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


# check_routing_q1 <- function(data) {
#
#   skipped_cols <- c(list("Q2.", "Q3.", "Q5."), grep("Q4.", colnames(data)))
#
#   for(col in skipped_cols){
#     data[, col] <- ifelse(data[,'Q1.'] != "Civil service, including desolved administations", NA, data[, col])
#   }
#
#   return(data)
#
# }
#
#
# check_routing_q3 <- function(data) {
#
#   skipped_cols = list("Q5.")
#
#   for(col in skipped_cols){
#     data[, col] <- ifelse(data[,'Q3.'] != "Office for National Statistics", NA, data[, col])
#   }
#
#   return(data)
#
# }
#
#
# check_routing_q6 <- function(data) {
#
#   skipped_cols = list("Q7.", "Q8.", "Q9.", "Q10.", "Q11.", "Q12.", "Q13.", "Q14.", "Q15.")
#
#   for(col in skipped_cols){
#     data[, col] <- ifelse(data[,'Q6.'] == "Any other qualification", NA, data[, col])
#   }
#
#   return(data)
#
# }
#
#
# check_routing_q16 <- function(data) {
#
#   skipped_cols = list("Q30.", "Q31.", "Q32.", "Q33.", "Q34.", "Q35.")
#
#   for(col in skipped_cols){
#     data[, col] <- ifelse(data[,'Q16.'] == "Never", NA, data[, col])
#   }
#
#   return(data)
#
# }
#
# check_routing_q22 <- function(data) {
#
#   skipped_cols = list("Q23.", "Q24.", "Q25.")
#
#   for(col in skipped_cols){
#     data[, col] <- ifelse(data[,'Q22.'] == "No", NA, data[, col])
#   }
#
#   return(data)
#
# }
