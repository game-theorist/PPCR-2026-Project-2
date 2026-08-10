#Diabetes

library(tidyverse)

#Loading raw data

diabetes_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DIQ_L.xpt")

#defining "refused" or "dont know"
diabetes_refused_or_dunno <- c(7, 9)

#Cleaning the data

diabetes_clean <- diabetes_raw |> 
  #selecting "Doctor told you have diabetes", "Ever told you have prediabetes", "Taking insulin now", "Take diabetic pills to lower blood sugar"
  select(SEQN, DIQ010, DIQ160, DIQ050, DIQ070) |>
  #Setting ID as factor
  mutate(SEQN = as.factor(SEQN)) |> 
  #Setting "refused" and "dont know" to NA
  mutate(across(where(is.numeric), ~ replace_values(.x, diabetes_refused_or_dunno ~ NA))) |> 
  #Removing rows with all values as missing
  filter_out(if_all(where(is.numeric), ~. == NA)) |> 
  #Changing "No" answer from "2" to "0"
  mutate(across(where(is.numeric), ~ replace_values(.x, 2 ~ 0))) |> 
  #Categorizing diabetics (yes = 1, no = 0) based on if they answered YES to any of the questions
  mutate(Diabetic = if_else(
    if_any(everything(), ~ . == 1), 1, 0, missing = 0)) |> 
  group_by(SEQN) |> 
  summarize(Diabetic)

#Proportion of diabetics among the whole NHANES 2021-2023 sample

prop_diabetic <- diabetes_clean |> 
  summarize(mean(Diabetic))

view(diabetes_clean)
