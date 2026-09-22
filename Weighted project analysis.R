#Weighted project analysis

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
         WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA)

nhanes_project_2_weighted <- svydesign(
  data = nhanes_project_2_tests,
  id = ~SDMVPSU,           
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  nest = TRUE
)

nhanes_project_2_weighted_subsetted <- subset(nhanes_project_2_weighted, main_sample == 1)

svy_model <- svyglm(
  PHQ9_Score ~ sitting_time * who_guideline + Gender + Age + Race + Ratio_income_poverty,
  design = nhanes_project_2_weighted
)

svy_model_subsetted <- svyglm(
  PHQ9_Score ~ sitting_time * who_guideline + Gender + Age + Race + Ratio_income_poverty,
  design = nhanes_project_2_weighted_subsetted
)

tidy_svy_model_subsetted <- tidy(svy_model_subsetted, conf.level = 0.95, conf.int = TRUE)

summarized_svy_model_subsetted <- summary(svy_model_subsetted)

svy_model

svy_model_subsetted

summarized_svy_model_subsetted

tidy_svy_model_subsetted