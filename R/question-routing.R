

check_routing_all <- function(data, question, skipped_questions, condition, inverse=FALSE) {

  if(!inverse) {
    for(col in skipped_questions){
      data[, col] <- ifelse(data[, question] == condition, NA, data[, col])
    }
  }
  else {
    for(col in skipped_questions){
      data[, col] <- ifelse(data[, question] != condition, NA, data[, col])
    }
  }

  return(data)

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
