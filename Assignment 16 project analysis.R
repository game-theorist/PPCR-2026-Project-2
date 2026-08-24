#project analysis

library(broom)

#selecting variables
nhanes_project_2_tests <- nhanes_project_2 |> 
  mutate(sitting_time = sitting_time / 60) |> 
  select(Gender, Age, Race, Ratio_income_poverty, sitting_time, 
         PHQ9_Score, depression, who_guideline_total, who_guideline, smoker, alcohol_use, unemployed, comorbidity_burden)

confounder_candidate <- c("Gender", "as.factor(Race)", "smoker", "as.factor(alcohol_use)", "unemployed", "as.factor(comorbidity_burden)")

#unadjusted GLM for depression ~ sitting time

nhanes_project_2_glm_unadjusted <- tidy(glm(depression ~ sitting_time, data = nhanes_project_2_tests, family = binomial),
                                        exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)

#adjusted GLM

nhanes_project_2_glm_adjusted <- tidy(glm(depression ~ sitting_time + smoker, data = nhanes_project_2_tests, family = binomial),
                                      exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)

# Loop through each variable to fit a model
models <- lapply(confounder_candidate, function(confounder) {
  glm_formula <- as.formula(paste("depression ~ sitting_time +", confounder))
  tidy(glm(glm_formula, data = nhanes_project_2_tests, family = binomial), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
})

# Assign names to the list for easy access
names(models) <- confounder_candidate

extracted_values <- map_dbl(models, ~ .x[[2, "estimate"]])

tibble(
  candidate = names(models),
  "odds_ratio" = extracted_values,
) |> 
  mutate("%_change" = abs(100 * ( 1 - (odds_ratio / nhanes_project_2_glm_unadjusted$estimate[2])))
  )





#project analysis

library(broom)

#selecting variables
nhanes_project_2_tests <- nhanes_project_2 |> 
  mutate(sitting_time = sitting_time / 60) |> 
  select(Gender, Age, Race, Ratio_income_poverty, sitting_time, 
         PHQ9_Score, depression, who_guideline_total, who_guideline, smoker, alcohol_use, unemployed, comorbidity_burden)

confounder_candidate <- c("Gender", "as.factor(Race)", "smoker", "as.factor(alcohol_use)", "unemployed", "as.factor(comorbidity_burden)")

#unadjusted LM for PHQ9_Score ~ sitting time

nhanes_project_2_lm_unadjusted <- tidy(lm(PHQ9_Score ~ sitting_time, data = nhanes_project_2_tests), conf.int = TRUE, conf.level = 0.95)

#adjusted LM

nhanes_project_2_lm_adjusted <- tidy(lm(PHQ9_Score ~ sitting_time + smoker, data = nhanes_project_2_tests), conf.int = TRUE, conf.level = 0.95)

# Loop through each variable to fit a model
models <- lapply(confounder_candidate, function(confounder) {
  lm_formula <- as.formula(paste("PHQ9_Score ~ sitting_time +", confounder))
  tidy(lm(lm_formula, data = nhanes_project_2_tests), conf.int = TRUE, conf.level = 0.95)
})

# Assign names to the list for easy access
names(models) <- confounder_candidate

extracted_values <- map_dbl(models, ~ .x[[2, "estimate"]])

tibble(
  candidate = names(models),
  "odds_ratio" = extracted_values,
) |> 
  mutate("%_change" = abs(100 * ( 1 - (odds_ratio / nhanes_project_2_lm_unadjusted$estimate[2])))
  )


