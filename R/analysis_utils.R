#' @title Compare the AIC scores of several models
#'
#' @param models a vector of fitted models
#'
#' @return data.frame object of model's AIC scores in ascending order
#'
#' @export

compare_models <- function(models){
  df <- data.frame(model = 1:length(models))
  df <- df %>%
    dplyr::mutate(aic = sapply(models, stats::AIC),
                  df_residual = sapply(models, function(x) x$df.residual)) %>%
    dplyr::arrange(aic)
  return(df)
}
