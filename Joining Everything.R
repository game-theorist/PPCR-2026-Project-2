#Joining everything

library(tidyverse)
library(haven)
library(psych)
library(skimr)

dep_pa_raw <- full_join(pa_moderate_vigorous_met, dep_cat_score, by = "SEQN")

dep_pa_demo_raw <- full_join(dep_pa_raw, demo_names, by = "SEQN")

dep_pa_demo_sitting_raw <- full_join(dep_pa_demo_raw, sitting_clean, by = "SEQN")

dep_pa_demo_sitting_diabetes_raw <- full_join(dep_pa_demo_sitting_raw, diabetes_clean, by = "SEQN")

#Filtering adults

dep_pa_demo_sitting_diabetes_adults <- dep_pa_demo_sitting_diabetes_raw |> 
  filter(Age %in% c(18:64))

#other filters

nhanes_project_2 <- dep_pa_demo_sitting_diabetes_adults |> 
  drop_na(PHQ9_Score) |> 
  filter_out(if_all(ends_with("minutes"), ~ is.na(.))
    ) 
  

view(nhanes_project_2)

#Splitting sitting time in quartiles

dep_pa_demo_sitting_sitting_quartiles <- nhanes_project_2 |> 
  mutate(sitting_quartile = ntile(sitting_time, 4))

dep_pa_demo_sitting_sitting_q1 <- dep_pa_demo_sitting_sitting_quartiles |> 
  filter(sitting_quartile == 1)

dep_pa_demo_sitting_sitting_q2 <- dep_pa_demo_sitting_sitting_quartiles |> 
  filter(sitting_quartile == 2)

dep_pa_demo_sitting_sitting_q3 <- dep_pa_demo_sitting_sitting_quartiles |> 
  filter(sitting_quartile == 3)

dep_pa_demo_sitting_sitting_q4 <- dep_pa_demo_sitting_sitting_quartiles |> 
  filter(sitting_quartile == 4)

#Splitting MET per week in quartiles

dep_pa_demo_sitting_met_quartiles <- nhanes_project_2 |> 
  mutate(MET_quartile = ntile(weekly_MET, 4))

dep_pa_demo_sitting_met_q1 <- dep_pa_demo_sitting_met_quartiles |> 
  filter(MET_quartile == 1)


dep_pa_demo_sitting_met_q2 <- dep_pa_demo_sitting_met_quartiles |> 
  filter(MET_quartile == 2)


dep_pa_demo_sitting_met_q3 <- dep_pa_demo_sitting_met_quartiles |> 
  filter(MET_quartile == 3)


dep_pa_demo_sitting_met_q4 <- dep_pa_demo_sitting_met_quartiles |> 
  filter(MET_quartile == 4)


#Plots

# Quartiles of MET per week x Depression

dep_pa_demo_sitting_met_quartiles <- nhanes_project_2 |> 
  mutate(MET_quartile = ntile(weekly_MET, 4))

dep_pa_demo_sitting_met_quartiles |> 
  ggplot(aes(x = MET_quartile, fill = Depression)) +
  geom_bar()

# Deciles of MET per week x Depression

dep_pa_demo_sitting_met_deciles <- nhanes_project_2 |> 
  mutate(MET_decile = ntile(weekly_MET, 10))

dep_pa_demo_sitting_met_deciles |> 
  ggplot(aes(x = MET_decile, fill = Depression)) +
  geom_bar()

# Quartiles of Daily sitting time X Depression

dep_pa_demo_sitting_sitting_quartiles <- nhanes_project_2 |> 
  mutate(sitting_quartile = ntile(sitting_time, 4))

dep_pa_demo_sitting_sitting_quartiles |> 
  ggplot(aes(x = sitting_quartile, fill = Depression)) +
  geom_bar()

# Deciles of daily sitting time x Depression

dep_pa_demo_sitting_sitting_deciles <- nhanes_project_2 |> 
  mutate(sitting_decile = ntile(sitting_time, 10))

dep_pa_demo_sitting_sitting_deciles |> 
  ggplot(aes(x = sitting_decile, fill = Depression)) +
  geom_bar()





nhanes_project_2 |> 
  ggplot(aes(x = weekly_MET)) +
  geom_histogram(binwidth = 80, boundary = 0) +
  scale_x_continuous(limits = c(0, 3000))

nhanes_project_2 |> 
  ggplot(aes(x = weekly_MET, color = Depression)) +
  geom_density() +
  scale_x_continuous(limits = c(0, 3000))


nhanes_project_2 |> 
  ggplot(aes(x = Depression, y = weekly_MET)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 5000))

nhanes_project_2 |> 
  ggplot(aes(x = weekly_MET, y = PHQ9_Score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(0, 5000))



nhanes_project_2 |> 
  ggplot(aes(x = moderate_weekly_minutes)) +
  geom_histogram(binwidth = 50, boundary = 0) +
  scale_x_continuous(limits = c(0, 2000))


nhanes_project_2 |> 
  ggplot(aes(x = vigorous_weekly_minutes)) +
  geom_histogram(binwidth = 50, boundary = 0) +
  scale_x_continuous(limits = c(0, 2000))

nhanes_project_2 |> 
  ggplot(aes(x = sitting_time)) +
  geom_histogram(binwidth = 60, boundary = 0) +
  scale_x_continuous(limits = c(0, 2000))


# Tests

#Linear model for PHQ9_Score ~ weekly_MET * sitting_time + Age + Gender

phq9_model <- lm(PHQ9_Score ~ weekly_MET * sitting_time + Age + Gender, data = nhanes_project_2)

tidyed_phq9_model <- tidy(phq9_model)

tidyed_phq9_model

#Linear model for PHQ9_Score ~ weekly_MET * sitting_time + Age + Gender in 1st quartile of sitting time

phq9_sitting_q1_model <- lm(PHQ9_Score ~ weekly_MET * sitting_time + Age + Gender, data = dep_pa_demo_sitting_sitting_q1)

tidyed_phq9_sitting_q1_model <- tidy(phq9_sitting_q1_model)

tidyed_phq9_sitting_q1_model

#Linear model for PHQ9_Score ~ weekly_MET * sitting_time + Age + Gender in 2nd quartile of sitting time

phq9_sitting_q2_model <- lm(PHQ9_Score ~ weekly_MET * sitting_time + Age + Gender, data = dep_pa_demo_sitting_sitting_q2)

tidyed_phq9_sitting_q2_model <- tidy(phq9_sitting_q2_model)

tidyed_phq9_sitting_q2_model

#Linear model for PHQ9_Score ~ weekly_MET * sitting_time + Age + Gender in 3rd quartile of sitting time

phq9_sitting_q3_model <- lm(PHQ9_Score ~ weekly_MET * sitting_time + Age + Gender, data = dep_pa_demo_sitting_sitting_q3)

tidyed_phq9_sitting_q3_model <- tidy(phq9_sitting_q3_model)

tidyed_phq9_sitting_q3_model

#Linear model for PHQ9_Score ~ weekly_MET * sitting_time + Age + Gender in 4th quartile of sitting time

phq9_sitting_q4_model <- lm(PHQ9_Score ~ weekly_MET * sitting_time + Age + Gender, data = dep_pa_demo_sitting_sitting_q4)

tidyed_phq9_sitting_q4_model <- tidy(phq9_sitting_q4_model)

tidyed_phq9_sitting_q4_model

#GLM ~ weekly_MET * sitting_time + Age + Gender

depression_model <- glm(Depression ~ weekly_MET * sitting_time + Age + Gender, data = nhanes_project_2)

tidyed_depression_model <- tidy(depression_model)

tidyed_depression_model

#GLM for Depression ~ weekly_MET * sitting_time + Age + Gender in 1st quartile of sitting time

depression_sitting_q1_model <- glm(Depression ~ weekly_MET * sitting_time + Age + Gender, data = dep_pa_demo_sitting_sitting_q1)

tidyed_depression_sitting_q1_model <- tidy(depression_sitting_q1_model)

tidyed_depression_sitting_q1_model

#GLM for Depression ~ weekly_MET * sitting_time + Age + Gender in 2nd quartile of sitting time

depression_sitting_q2_model <- glm(Depression ~ weekly_MET * sitting_time + Age + Gender, data = dep_pa_demo_sitting_sitting_q2)

tidyed_depression_sitting_q2_model <- tidy(depression_sitting_q2_model)

tidyed_depression_sitting_q2_model

#GLM for Depression ~ weekly_MET * sitting_time + Age + Gender in 3rd quartile of sitting time

depression_sitting_q3_model <- glm(Depression ~ weekly_MET * sitting_time + Age + Gender, data = dep_pa_demo_sitting_sitting_q3)

tidyed_depression_sitting_q3_model <- tidy(depression_sitting_q3_model)

tidyed_depression_sitting_q3_model

#GLM for Depression ~ weekly_MET * sitting_time + Age + Gender in 3rd quartile of sitting time

depression_sitting_q4_model <- glm(Depression ~ weekly_MET * sitting_time + Age + Gender, data = dep_pa_demo_sitting_sitting_q4)

tidyed_depression_sitting_q4_model <- tidy(depression_sitting_q4_model)

tidyed_depression_sitting_q4_model