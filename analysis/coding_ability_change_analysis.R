library(magrittr)
library(CARS)
library(car)
library(MASS)
library(stats)
library(Hmisc)
library(brant)
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

t$capability_change_by_freq %>% tidyr::pivot_wider(names_from = code_freq, values_from = n) %>% data.frame()

CARS::plot_likert(t$capability_change_by_freq, mid = 3, neutral_mid = TRUE, xlab = "%", font_size = 14, height = 600, width = "100%")


t$capability_change_by_line_manage %>% tidyr::pivot_wider(names_from = management, values_from = n) %>% data.frame()

CARS::plot_likert(t$capability_change_by_line_manage, mid = 3, neutral_mid = TRUE, xlab = "%", font_size = 14, height = 600, width = "100%")


t$capability_change_by_CS_grade %>% tidyr::pivot_wider(names_from = CS_grade, values_from = n) %>% data.frame()

CARS::plot_likert(t$capability_change_by_CS_grade, mid = 3, neutral_mid = TRUE, xlab = "%", font_size = 14, height = 600, width = "100%")

# ===========
# Encode data
# ===========

selected_data <- subset(data, select=c("coding_ability_change", "code_freq", "management", "CS_grade", "prof_DS", "prof_DDAT"))

selected_data <- selected_data %>%
  dplyr::mutate(coding_ability_change = dplyr::case_when(coding_ability_change %in% c("Slightly worse",
                                                                                      "Significantly worse") ~ "Worse",
                                                         TRUE ~ coding_ability_change))

selected_data$coding_ability_change <- ordered(selected_data$coding_ability_change,
                                              levels = c("Worse",
                                                         "No change",
                                                         "Slightly better",
                                                         "Significantly better"),
                                              exclude = NULL)

selected_data <- selected_data %>%
  dplyr::mutate(code_freq = dplyr::case_when(code_freq %in% c("Never",
                                                              "Rarely") ~ "Rarely or never",
                                             TRUE ~ code_freq))

selected_data$code_freq <- factor(selected_data$code_freq,
                                  levels = c("Rarely or never",
                                             "Sometimes",
                                             "Regularly",
                                             "All the time"),
                                  exclude = NULL)

selected_data <- selected_data %>%
  dplyr::mutate(manage_coders = ifelse(management == "Yes", "Yes", "No")) %>%
  dplyr::mutate(manage_non_coders = ifelse(management == "No - I manage people who do not write code", "Yes", "No"))

selected_data$manage_coders <- factor(selected_data$manage_coders)
selected_data$manage_non_coders <- factor(selected_data$manage_non_coders)

selected_data <- selected_data %>%
  dplyr::mutate(CS_grade = dplyr::case_when(CS_grade %in% c("Grade 7 (or equivalent)",
                                                            "Grade 6 (or equivalent)") ~ "Grade 6 and 7",
                                            TRUE ~ CS_grade))

selected_data$CS_grade <- factor(selected_data$CS_grade,
                                 levels = c("Higher Executive Officer (or equivalent)",
                                            "Senior Executive Officer (or equivalent)",
                                            "Grade 6 and 7"),
                                 exclude = NULL)

selected_data <- selected_data %>%
  dplyr::mutate(data_profession = ifelse(prof_DS=="Yes" | prof_DDAT=="Yes", "Yes", "No"))

selected_data$data_profession <- factor(selected_data$data_profession)

selected_data <- dplyr::select(selected_data, coding_ability_change, code_freq,
                               manage_coders, manage_non_coders, CS_grade, data_profession)

selected_data <- tidyr::drop_na(selected_data)

# ==============================================================
# Test multicolinearity assumption for logistic regression holds
# ==============================================================

r <- cor(data.matrix(selected_data), use = "complete.obs", method = "kendall")

ggcorrplot(r,
           type = "lower",
           lab = TRUE)

# =================================
# Fitting ordinal regression models
# =================================

M1 <- polr(coding_ability_change~code_freq+manage_coders+manage_non_coders+CS_grade+data_profession, data = selected_data,
           Hess = TRUE, method="logistic")

M2 <- polr(coding_ability_change~code_freq+manage_non_coders+CS_grade+data_profession, data = selected_data,
           Hess = TRUE, method="logistic")

M3 <- polr(coding_ability_change~code_freq+CS_grade+data_profession, data = selected_data,
           Hess = TRUE, method="logistic")

M4 <- polr(coding_ability_change~code_freq+CS_grade, data = selected_data,
           Hess = TRUE, method="logistic")

M5 <- polr(coding_ability_change~code_freq, data = selected_data,
           Hess = TRUE, method="logistic")

M6 <- polr(coding_ability_change~code_freq*(manage_coders+manage_non_coders+CS_grade+data_profession), data = selected_data,
           Hess = TRUE, method="logistic")

M7 <- polr(coding_ability_change~code_freq*(CS_grade+data_profession), data = selected_data,
           Hess = TRUE, method="logistic")

M8 <- polr(coding_ability_change~code_freq*CS_grade, data = selected_data,
           Hess = TRUE, method="logistic")

M9 <- polr(coding_ability_change~code_freq*(data_profession+CS_grade+manage_non_coders)+CS_grade*manage_coders, data = selected_data,
            Hess = TRUE, method="logistic")

# ===========================
# Compare models for best fit
# ===========================

models <- list(M1, M2, M3, M4, M5, M6, M7, M8, M9)

CARS::compare_models(models)

# ====================================================
# Generate p-values and odds ratios for best fit model
# ====================================================

# get coefficients (it's in matrix form)
coefficients <- summary(M7)$coefficients

# calculate p-values
p_value <- (1 - pnorm(abs(coefficients[ ,"t value"]), 0, 1))*2

# bind back to coefficients
(coefficients <- cbind(coefficients, p_value))

# calculate odds ratios
odds_ratio <- exp(coefficients[ ,"Value"])

# combine with coefficient and p_value
(coefficients <- cbind(
  coefficients[ ,c("Value", "p_value")],
  odds_ratio
))

# ===============================================================
# Test proportional odds assumption for logistic regression holds
# ===============================================================

brant(M7)

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

(s <- with(selected_data, summary(as.numeric(coding_ability_change) ~ code_freq*(CS_grade+data_profession), fun=sf)))

plot(s, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:5]))
