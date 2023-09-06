library(magrittr)
library(CARS)
library(nnet)
library(ggplot2)
library(ggcorrplot)

ONS_data <- CARS::get_tidy_data() %>%
  CARS::rename_cols() %>%
  CARS::apply_skip_logic() %>%
  CARS::clean_departments() %>%
  CARS::derive_vars() %>%
  dplyr::filter(department == "Office for National Statistics")

t <- CARS::summarise_all(ONS_data, all_tables = TRUE)

RAP_strategy_data <- ONS_data %>%
  dplyr::select("strategy_knowledge", "prof_DS", "prof_DDAT",
                "prof_GES", "prof_geog", "prof_GSR", "prof_GSG") %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(everything(), as.factor)) %>%
data.frame(row.names = NULL)

RAP_strategy_data$strategy_knowledge <- relevel(RAP_strategy_data$strategy_knowledge,
                                                ref = "I have not heard of the RAP strategy")

MN1 <- multinom(formula = strategy_knowledge ~ ., data = RAP_strategy_data, HESS=TRUE)
MN2 <- multinom(formula = strategy_knowledge ~ . -prof_geog, data = RAP_strategy_data, HESS=TRUE)
MN3 <- multinom(formula = strategy_knowledge ~ . -prof_geog -prof_GES, data = RAP_strategy_data, HESS=TRUE)
MN4 <- multinom(formula = strategy_knowledge ~ . -prof_geog -prof_GES -prof_DDAT, data = RAP_strategy_data, HESS=TRUE)

models <- list(MN1, MN2, MN3, MN4)

CARS::compare_models(models)

summary(MN4)

exp(coef(MN4))
