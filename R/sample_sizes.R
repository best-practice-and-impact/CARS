#' @title Sample sizes for table/plot outputs
#'
#' @param data full CARS dataset after cleaning
#'
#' @return list of sample sizes
#'
#' @export
#'

expected_sample_sizes <- function(data) {
  list(
    all = nrow(data),
    cs = sum(data$workplace == "Civil service, including devolved administrations"),
    coder = sum(data$coding_exp == "Yes"),
    non_coder = sum(data$coding_exp == "No"),
    code_at_work = sum(data$coding_exp == "Yes" & !is.na(data$code_freq) & data$code_freq != "Never"),
    ability_change = sum(data$coding_exp == "Yes" & data$first_learned != "Current role"),
    heard_of_rap = sum(data$heard_of_rap == "Yes"),
    use_ai =  sum(data$coding_exp == "Yes" & !is.na(data$code_freq) & data$code_freq != "Never" & data$ai == "Yes")

  )
}
