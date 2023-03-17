library(magrittr)
library(CARS)
library(nnet)
library(ggplot2)
library(ggcorrplot)

data <- CARS::get_tidy_data() %>%
  CARS::rename_cols() %>%
  CARS::apply_skip_logic() %>%
  CARS::clean_departments() %>%
  CARS::derive_vars()

t <- CARS::summarise_all(data, all_tables = TRUE)

# ===============
# Cross-tab plots
# ===============

CARS::plot_freqs(t$heard_of_RAP_by_prof, xlab = "Profession", ylab = "Heard of RAP (%)", font_size = 14)

# ===========
# Encode data
# ===========

RAP_awareness_data <- data %>%
  dplyr::select("heard_of_RAP", "department", "prof_DS", "prof_DDAT", "prof_GAD",
                "prof_GES", "prof_geog", "prof_GORS", "prof_GSR", "prof_GSG") %>%
  tidyr::drop_na() %>%
  dplyr::mutate(department = ifelse(department == "Office for National Statistics",
                                    "Yes", "No"),
                across(everything(), as.factor)) %>%
  dplyr::rename(ONS = department) %>%
  data.frame(row.names = NULL)

# ==============================================================
# Test multicolinearity assumption for logistic regression holds
# ==============================================================

r <- cor(data.matrix(RAP_awareness_data), use = "complete.obs", method = "kendall")

ggcorrplot(r,
           type = "lower",
           lab = TRUE)

# ============================================
# Fitting generalized linear regression models
# ============================================

GLM1 <- glm(formula = heard_of_RAP ~ ., data = RAP_awareness_data, family = "binomial")
GLM2 <- glm(formula = heard_of_RAP ~ . -prof_geog, data = RAP_awareness_data, family = "binomial")
GLM3 <- glm(formula = heard_of_RAP ~ . -prof_geog -prof_GSR, data = RAP_awareness_data, family = "binomial")
GLM4 <- glm(formula = heard_of_RAP ~ . -prof_geog -prof_GSR -prof_GES, data = RAP_awareness_data, family = "binomial")
GLM5 <- glm(formula = heard_of_RAP ~ . -prof_geog -prof_GSR -prof_GES -prof_DDAT, data = RAP_awareness_data, family = "binomial")
GLM6 <- glm(formula = heard_of_RAP ~ . -prof_geog -prof_GSR -prof_GES -prof_DDAT -prof_GAD, data = RAP_awareness_data, family = "binomial")
GLM7 <- glm(formula = heard_of_RAP ~ . -prof_geog -prof_DDAT, data = RAP_awareness_data, family = "binomial")

models <- list(GLM1, GLM2, GLM3, GLM4, GLM5, GLM6, GLM7)

CARS::compare_models(models)

summary(GLM4)

# =======================================
# Generate odds ratios for best fit model
# =======================================

# calculate odds ratios
odds_ratio <- exp(coefficients(GLM4))

# bind back to coefficients
(coefficients <- round(cbind(summary(GLM4)$coefficients, odds_ratio), 4))


# ===========
# Encode data
# ===========

RAP_score_data <- data %>%
  dplyr::select("heard_of_RAP", "basic_rap_score", "advanced_rap_score") %>%
  tidyr::drop_na() %>%
  dplyr::mutate(across(everything(), as.factor)) %>%
  data.frame(row.names = NULL)

# ==============================================================
# Test multicolinearity assumption for logistic regression holds
# ==============================================================

r <- cor(data.matrix(RAP_score_data), use = "complete.obs", method = "kendall")

ggcorrplot(r,
           type = "lower",
           lab = TRUE)

# ============================================
# Fitting generalized linear regression models
# ============================================

BGLM <- glm(formula = basic_rap_score ~ heard_of_RAP, data = RAP_score_data, family = "binomial")
AGLM <- glm(formula = advanced_rap_score ~ heard_of_RAP, data = RAP_score_data, family = "binomial")

summary(BGLM)
summary(AGLM)

