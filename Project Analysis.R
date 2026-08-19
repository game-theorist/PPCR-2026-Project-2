#project analysis

library(broom)

#selecting variables
nhanes_project_2_tests <- nhanes_project_2 |> 
  mutate(sitting_time = sitting_time / 60) |> 
  select(Gender, Age, Race, Ratio_income_poverty, sitting_time, 
         PHQ9_Score, depression, who_guideline_total, who_guideline, smoker, alcohol_use, unemployed, comorbidity_burden)

#unadjusted GLM for depression ~ sitting time

nhanes_project_2_glm_unadjusted <- glm(depression ~ sitting_time, data = nhanes_project_2_tests, family = binomial)

#adjusted GLM

nhanes_project_2_glm_adjusted <- glm(depression ~ sitting_time + Gender + as.factor(Race) + smoker + as.factor(alcohol_use) + unemployed + as.factor(comorbidity_burden), 
    data = nhanes_project_2_tests, family = binomial)

tidy_nhanes_project_2_glm_unadjusted <- tidy(nhanes_project_2_glm_unadjusted, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)

tidy_nhanes_project_2_glm_adjusted <- tidy(nhanes_project_2_glm_adjusted, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)


tidy_nhanes_project_2_glm_unadjusted

tidy_nhanes_project_2_glm_adjusted