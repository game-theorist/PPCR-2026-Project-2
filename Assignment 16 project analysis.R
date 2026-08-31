#project analysis

library(broom)
library(psych)
library(car)

#selecting variables
nhanes_project_2_tests <- nhanes_project_2 |> 
  mutate(sitting_time = sitting_time / 60) |> 
  select(Gender, Age, Race, Ratio_income_poverty, sitting_time, 
         PHQ9_Score, depression, who_guideline_total, who_guideline, smoker, alcohol_use, unemployed, comorbidity_burden)

confounder_candidate <- c("Gender", "as.factor(Race)", "smoker", "as.factor(alcohol_use)", "unemployed", "as.factor(comorbidity_burden)")

nhanes_project_2_glm_unadjusted <- tidy(glm(depression ~ sitting_time, data = nhanes_project_2_tests, family = binomial), exponentiate = TRUE)

# Loop through each variable to fit a model
untidy_models <- lapply(confounder_candidate, function(confounder) {
  glm_formula <- as.formula(paste("depression ~ sitting_time +", confounder))
  glm(glm_formula, data = nhanes_project_2_tests, family = binomial)
})


models <- lapply(confounder_candidate, function(confounder) {
  glm_formula <- as.formula(paste("depression ~ sitting_time +", confounder))
  tidy(glm(glm_formula, data = nhanes_project_2_tests, family = binomial), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
})

# Assign names to the list for easy access
names(models) <- confounder_candidate

extracted_values <- (map_dbl(models, ~ .x[[2, "estimate"]]))

odds_ratio_change <- tibble(
  candidate = names(models),
  "odds_ratio" = round(extracted_values, digits = 6)
) |> 
  mutate("%_change" = abs(100 * ( 1 - (odds_ratio / nhanes_project_2_glm_unadjusted$estimate[2]))),
         n = sapply(untidy_models, nobs)
  )

#assignment final model

untidy_final_assignment_16_model <- glm(depression ~ sitting_time + Gender, data = nhanes_project_2_tests, family = binomial)

final_assignment_16_model <- tidy(glm(depression ~ sitting_time + Gender, data = nhanes_project_2_tests, family = binomial), exponentiate = TRUE)

#colinearity

correlation <- corr.test(nhanes_project_2_tests[, c("depression", "sitting_time", "Gender")], use = "pairwise")

pwcorr <- pwcorr(nhanes_project_2_tests, vars = c("depression", "sitting_time", "Gender"))

vif <- vif(untidy_final_assignment_16_model)

models

odds_ratio_change

pwcorr

vif

augment(untidy_final_assignment_16_model) |> 
  count(depression)
