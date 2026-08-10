

library(tidyverse)
library(haven)
library(psych)
library(skimr)

#Loading raw data

demographics_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DEMO_L.xpt")
depression_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DPQ_L.xpt")
diabetes_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DIQ_L.xpt")
pa_sitting_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/PAQ_L.xpt")
smoking_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/SMQ_L.xpt")
unemployment_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/OCQ_L.xpt")

#Joining the raw data into a single dataframe

nhanes_project_2_raw <- demographics_raw |> 
  full_join(pa_sitting_raw, by = "SEQN") |> 
  full_join(depression_raw, by = "SEQN") |> 
  full_join(diabetes_raw, by = "SEQN") |> 
  full_join(smoking_raw, by = "SEQN") |> 
  full_join(unemployment_raw, by = "SEQN") |> 
  zap_labels()

         
#Defining "refused" and "dont know"

depression_refused_dunno <- c(7, 9)
pa_refused_or_dunno <- c(7777, 9999)
smoking_refused_or_dunno <- c(7, 9)
diabetes_refused_or_dunno <- c(7, 9)
OCD150_refused_or_dunno <- c(7, 9)
OCQ383_refused_or_dunno <- c(77, 99)


#Cleaning the data

nhanes_project_2 <- nhanes_project_2_raw |> 
  
  #Selecting variables of interest
  select(
    #Demographics variables
    SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, WTINT2YR, INDFMPIR,
    #Depression variables
    c(DPQ010:DPQ100), 
    #Physical activity and sitting variables
    c(PAD790Q:PAD680),
    #Smoking variables
    SMQ040,
    #Unemployment variables
    OCD150, OCQ383,
    #Diabetes variables
    DIQ010, DIQ160, DIQ050, DIQ070
    ) |>
  
  #Demographics data cleaning
  
  #Renaming variables and setting values for gender (Male = 1, Female = 0)
  mutate(Gender = if_else(RIAGENDR == 1, 1, 0)) |> 
  rename(Age = RIDAGEYR,
         Race = RIDRETH3,
         Interview_Weight = WTINT2YR,
         Ratio_income_poverty = INDFMPIR
         ) |> 
  
  #Depression data cleaning
  
  #Calculating PHQ-9 Score
  mutate(across(c(DPQ010:DPQ090), ~ replace_values(.x, depression_refused_dunno ~ NA))) |> 
  filter(!if_any(c(DPQ010:DPQ090), is.na)) |> 
  mutate(PHQ9_Score = rowSums(across(DPQ010:DPQ090), na.rm = TRUE)) |> 
  #Categorizing depression yes/no (score >= 10)
  mutate(Depression = if_else(PHQ9_Score >= 10, 1, 2)) |> 
  #Removing unnecessary depression variables
  select(!c(DPQ010:DPQ090)) |> 
  
  #Physical activity data cleaning
  
  #Setting factor variables as factors
  mutate(PAD790U = as.factor(PAD790U),
         PAD810U = as.factor(PAD810U)) |> 
  #Setting "refused", "dont know" or "0" as missing
  mutate(across(!c(SEQN, PAD790U, PAD810U, PAD680), ~ replace_values(.x, pa_refused_or_dunno ~ NA))) |> 
  #Removing observations with all values of physical activity and sitting time variables as missing
  filter(!if_all(c(PAD790Q, PAD800, PAD810Q, PAD820), is.na)) |> 
  #Removing observations with missing sitting time
  filter(!is.na(PAD680)) |> 
  #Renaming sitting time
  rename("sitting_time" = "PAD680") |>
  #Transforming observation data to minutes_per_week of moderate activity
  mutate(moderate_weekly_minutes = case_when(
    PAD790U == "D" ~ PAD800 * 7,
    PAD790U == "M" ~ PAD800 / 4,
    PAD790U == "Y" ~ PAD800 / 52,
    PAD790U == "W" ~ PAD800)
  ) |> 
  #calculating weekly MET-minutes
  mutate(moderate_met = moderate_weekly_minutes * 5) |> 
  #Transforming observation data to minutes_per_week of vigorous activity
  mutate(vigorous_weekly_minutes = case_when(
    PAD810U == "D" ~ PAD820 * 7,
    PAD810U == "M" ~ PAD820 / 4,
    PAD810U == "Y" ~ PAD820 / 52,
    PAD810U == "W" ~ PAD820)) |> 
  #Calculating weekly MET-minutes
  mutate(vigorous_met = vigorous_weekly_minutes * 7) |>
  #Summing up MET (vigorous + moderate)
  mutate(weekly_MET = rowSums(pick(moderate_met, vigorous_met), na.rm = TRUE)) |>
  #Summing moderate (* 1) and vigorous (* 2) for WHO guideline equivalency
  mutate(who_guideline_total = 
           coalesce(moderate_weekly_minutes, 0) + 
           coalesce(vigorous_weekly_minutes, 0) * 2
  ) |> 
  #WHO guideline (150 moderate minutes or 75 vigorous minutes or equivalent; yes or no
  mutate(who_guideline = if_else(who_guideline_total >= 150, 1, 0)) |> 
  #Dropping unnecessary variables
  select(!starts_with("PAD")) |> 
  
  #Smoking data cleaning
  
  #Setting "refused" and "dont know" to NA
  mutate(across(SMQ040, ~ replace_values(.x, smoking_refused_or_dunno ~ NA))) |> 
  #Replacing 2 for "No" with 0
  mutate(smoker = if_else(SMQ040 == 2, 0, 1)) |> 
  #Removing unnecessary variables
  select(!SMQ040) |> 
  
  #Unemployment data cleaning
  
  #Setting OCD150 and OCQ383 "refused" and "dont know" to NA
  mutate(across(OCD150, ~ replace_values(.x, OCD150_refused_or_dunno ~ NA))) |>
  mutate(across(OCQ383, ~ replace_values(.x, OCQ383_refused_or_dunno ~ NA))) |> 
  #Removing observations with unemployment data
  filter(!is.na(OCD150)) |> 
  #Removing subjects with missing values in OCQ383 IF OCD150 = 4
  filter(!when_all(OCD150 %in% c(4) & is.na(OCQ383))) |> 
  #Setting employed subjects as 0 and unemployed as 1
  mutate(unemployed = case_when(OCQ383 %in% c(5, 6, 7) ~ 1,
                                   OCD150 == 3 ~ 1,
                                   .default = 0)
         ) |>
  #Removing unnecessary variables
  select(!c(OCD150, OCQ383)) |> 
  
  #Diabetes data cleaning
  
  #Setting "refused" and "dont know" to NA
  mutate(across(c(DIQ010, DIQ160, DIQ050, DIQ070), ~ replace_values(.x, diabetes_refused_or_dunno ~ NA))) |> 
  #Removing rows with all values as missing
  filter(!if_all(c(DIQ010, DIQ160, DIQ050, DIQ070), is.na)) |> 
  #Changing "No" answer from "2" to "0"
  mutate(across(c(DIQ010, DIQ160, DIQ050, DIQ070), ~ replace_values(.x, 2 ~ 0))) |> 
  #Categorizing diabetics (yes = 1, no = 0) based on if they answered YES to ANY of the questions
  mutate(Diabetic = if_else(
    if_any(everything(), ~ . == 1), 1, 0, missing = 0)) |>
  #Removing unnecessary variables
  select(!c(DIQ010, DIQ160, DIQ050, DIQ070))

view(nhanes_project_2)
