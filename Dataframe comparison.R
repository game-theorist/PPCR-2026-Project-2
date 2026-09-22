library(readxl)

#Group 12 dataframe

nhanes_group_12_raw <- read_excel("G:/My Drive/PPCR/Project 2 - NHANES/Group12_Milestone4_Final_numeric.xlsx")

View(nhanes_group_12_raw)

#Adapting my dataframe

nhanes_project_2_adapted <- nhanes_project_2 |> 
  mutate(Gender = if_else(Gender == 0, 2, 1),
         sitting_hours = sitting_time %/% 60
         ) |>
  rename(
    seqn = SEQN,
    riagendr = Gender,
    ridageyr = Age,
    ridreth3 = Race,
    phq9_total = PHQ9_Score,
    indfmpir = Ratio_income_poverty,
    who_pa = who_guideline,
    employment_status = unemployed
  ) |> 
  select(!c(sitting_time, moderate_weekly_minutes, moderate_met, vigorous_weekly_minutes, vigorous_met, weekly_MET, eq_vigorous_weekly_minutes, 
            who_guideline_total))

#Comparisons

nhanes_project_2_comparison <- nhanes_group_12_raw |> 
  full_join(nhanes_project_2_adapted, by = "seqn") |> 
  filter(smoker != smoking_status) |> 
  relocate(smoker, smoking_status)

View (nhanes_project_2_comparison)
View (pa_sitting_raw)
