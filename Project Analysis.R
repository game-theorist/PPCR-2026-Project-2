#project analysis

library(tidyverse)
library(broom)
library(psych)
library(car)
library(describedata)
library(estimatr)
library(survey)

#selecting variables
nhanes_project_2_tests <- nhanes_project_2 |>  
  mutate(
    PHQ9_Score = log1p(PHQ9_Score),
    sitting_time = sitting_time / 60,
    Race = as.factor(Race),
    who_guideline = as.factor(who_guideline)
  ) |> 
  select(main_sample, Gender, Age, Race, Ratio_income_poverty, sitting_time, 
         PHQ9_Score, depression, who_guideline_total, who_guideline, smoker, alcohol_use, unemployed, comorbidity_burden,
         WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA) |> 
  filter(main_sample == 1)

confounder_candidate <- c("Gender", "Age", "as.factor(Race)", "as.factor(Ratio_income_poverty)", "smoker", "as.factor(alcohol_use)", "unemployed", "as.factor(comorbidity_burden)")

nhanes_project_2_lm_unadjusted <- tidy(lm(PHQ9_Score ~ sitting_time, data = nhanes_project_2_tests))

# Loop through each variable to fit a model
untidy_models <- lapply(confounder_candidate, function(confounder) {
  lm_formula <- as.formula(paste("PHQ9_Score ~ sitting_time +", confounder))
  lm(lm_formula, data = nhanes_project_2_tests)
})


models <- lapply(confounder_candidate, function(confounder) {
  lm_formula <- as.formula(paste("PHQ9_Score ~ sitting_time +", confounder))
  tidy(lm(lm_formula, data = nhanes_project_2_tests))
})

# Assign names to the list for easy access
names(models) <- confounder_candidate

extracted_values <- (map_dbl(models, ~ .x[[2, "estimate"]]))

odds_ratio_change <- tibble(
  candidate = names(models),
  "odds_ratio" = round(extracted_values, digits = 6)
) |> 
  mutate("%_change" = abs(100 * ( 1 - (odds_ratio / nhanes_project_2_lm_unadjusted$estimate[2]))),
         n = sapply(untidy_models, nobs)
  )

#Effect modification

untidy_final_assignment_18_model <- lm(PHQ9_Score ~ sitting_time * who_guideline + Gender + Age + Race + Ratio_income_poverty, data = nhanes_project_2_tests)

final_assignment_18_model <- tidy(lm(PHQ9_Score ~ sitting_time * who_guideline + Gender + Age + Race + Ratio_income_poverty, data = nhanes_project_2_tests), conf.int = TRUE, conf.level = 0.95)

#colinearity

#correlation <- corr.test(nhanes_project_2_tests[, c("PHQ9_Score", "sitting_time", "Gender", "Age", "Race", "Ratio_income_poverty")], use = "pairwise")

pwcorr_data <- nhanes_project_2_tests |> 
  mutate(
    who_guideline = as.numeric(who_guideline),
    Race = as.numeric(Race)
  )
pwcorr <- pwcorr(pwcorr_data, vars = c("PHQ9_Score", "sitting_time", "who_guideline", "Gender", "Age", "Race", "Ratio_income_poverty"))

vif <- vif(untidy_final_assignment_18_model)

augmented_model <- augment(untidy_final_assignment_18_model)

summarized_model <- summary(untidy_final_assignment_18_model)

models

odds_ratio_change

pwcorr

vif

final_assignment_18_model

summarized_model
