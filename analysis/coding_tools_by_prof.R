library(magrittr)
library(CARS)
library(nnet)

data <- CARS::get_tidy_data() %>%
  CARS::rename_cols() %>%
  CARS::apply_skip_logic() %>%
  CARS::clean_departments() %>%
  CARS::derive_vars()

ONS_data <- dplyr::filter(data, department == "Office for National Statistics")

t <- CARS::summarise_all(data, all_tables = TRUE)

ONS_t <- CARS::summarise_all(ONS_data, all_tables = TRUE)


CARS::freq_subplots(t$languages_by_prof, "", "", y_margin = .1,
                    x_margin = .15, orientation = "h", height = 1500,
                    width = 800, nrows = 4, font_size = 14)

CARS::plot_likert(ONS_t$open_source_by_prof, mid = 4, neutral_mid = FALSE,
                  xlab = "%", font_size = 14, height = 600, width = "100%")


selected_data <- data %>%
  dplyr::select("knowledge_R", "knowledge_python", "department", "prof_DS", "prof_DDAT",
                "prof_GAD", "prof_GES", "prof_geog", "prof_GORS", "prof_GSR",
                "prof_GSG") %>%
  dplyr::mutate(department = ifelse(department == "Office for National Statistics",
                                    "Yes", "No"),
                knowledge_opensource = ifelse(knowledge_R == "Yes" |
                                                knowledge_python == "Yes",
                                              "Yes",
                                              "No"),
                across(everything(), ~factor(.x, levels = c("No", "Yes")))) %>%
  dplyr::rename(ONS = department) %>%
  tidyr::drop_na() %>%
  data.frame(row.names = NULL)

GLM1 <- glm(formula = knowledge_opensource ~ . -knowledge_R -knowledge_python, data = selected_data, family = "binomial")
GLM2 <- glm(formula = knowledge_opensource ~ . -knowledge_R -knowledge_python -prof_DDAT, data = selected_data, family = "binomial")
GLM3 <- glm(formula = knowledge_opensource ~ . -knowledge_R -knowledge_python -prof_DDAT -prof_GAD, data = selected_data, family = "binomial")
GLM4 <- glm(formula = knowledge_opensource ~ . -knowledge_R -knowledge_python -prof_DDAT -prof_GAD -prof_geog, data = selected_data, family = "binomial")
GLM5 <- glm(formula = knowledge_opensource ~ . -knowledge_R -knowledge_python -prof_DDAT -prof_GAD -prof_geog -prof_GSR, data = selected_data, family = "binomial")

models <- list(GLM1, GLM2, GLM3, GLM4, GLM5)

CARS::compare_models(models)

summary(GLM4)

# =======================================
# Generate odds ratios for best fit model
# =======================================

# calculate odds ratios
odds_ratio <- exp(coefficients(GLM4))

# bind back to coefficients
(coefficients <- round(cbind(summary(GLM4)$coefficients, odds_ratio), 4))

friedman.test(n ~ value | name, data = ONS_t$open_source_by_prof)

pairwise.wilcox.test(ONS_t$open_source_by_prof$n, ONS_t$open_source_by_prof$value, p.adj = "bonf")


