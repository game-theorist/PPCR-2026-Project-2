
library(tidyverse)
library(haven)
library(psych)
library(skimr)
library(GGally)

#Loading raw data

demographics_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DEMO_L.xpt")
depression_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DPQ_L.xpt")
diabetes_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DIQ_L.xpt")
pa_sitting_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/PAQ_L.xpt")
smoking_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/SMQ_L.xpt")
alcohol_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/ALQ_L.xpt")
unemployment_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/OCQ_L.xpt")
bloodpressure_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/BPXO_L.xpt")
bpcholesterol_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/BPQ_L.xpt")
comorbidities_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/MCQ_L.xpt")

#Joining the raw data into a single dataframe

nhanes_project_2_raw <- demographics_raw |> 
  full_join(pa_sitting_raw, by = "SEQN") |> 
  full_join(depression_raw, by = "SEQN") |> 
  full_join(diabetes_raw, by = "SEQN") |> 
  full_join(smoking_raw, by = "SEQN") |>
  full_join(alcohol_raw, by = "SEQN") |> 
  full_join(unemployment_raw, by = "SEQN") |> 
  full_join(bloodpressure_raw, by = "SEQN") |> 
  full_join(bpcholesterol_raw, by = "SEQN") |> 
  full_join(comorbidities_raw, by = "SEQN") |> 
  zap_labels()


#Defining "refused" and "dont know"

depression_refused_dunno <- c(7, 9)
pa_refused_or_dunno <- c(7777, 9999)
smoking_refused_or_dunno <- c(7, 9)
alcohol_small_refused_or_dunno <- c(7, 9)
alcohol_medium_refused_or_dunno <- c(77, 99)
alcohol_large_refused_or_dunno <- c(777, 999)
diabetes_refused_or_dunno <- c(7, 9)
bpcholesterol_refused_or_dunno <- c(7, 9)
comorbidities_refused_or_dunno <- c(7, 77, 9, 99)
OCD150_refused_or_dunno <- c(7, 9)
OCQ383_refused_or_dunno <- c(77, 99)


#Cleaning the data

nhanes_project_2 <- nhanes_project_2_raw |> 
  
  #Selecting variables of interest
  select(
    #Demographics variables
    SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, INDFMPIR, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA,
    #depression variables
    c(DPQ010:DPQ100), 
    #Physical activity and sitting variables
    c(PAD790Q:PAD680),
    #Smoking variables
    SMQ020, SMQ040,
    #Alcohol use variables
    ALQ111, ALQ121, ALQ130,
    #Unemployment variables
    OCD150, OCQ383,
    #Diabetes variables
    DIQ010, DIQ160, DIQ050, DIQ070,
    #Blood pressure variables
    BPXOSY1, BPXOSY2, BPXOSY3, BPXODI1, BPXODI2, BPXODI3, BPQ020, BPQ030, BPQ150,
    #Cholesterol variables
    BPQ080, BPQ101D,
    #Comorbidities variables
    MCQ010, MCQ035, MCQ040, MCQ050, MCQ160A, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F, MCQ160M, MCQ170M, MCQ160P, MCQ160L, MCQ170L, MCQ220
  ) |>
  
  #Demographics data cleaning
  
  #Renaming variables
  rename(Gender = RIAGENDR,
         Age = RIDAGEYR,
         Race = RIDRETH3,
         Ratio_income_poverty = INDFMPIR
  ) |>
  #Setting values for gender (Male = 1, Female = 0)
  mutate(Gender = if_else(Gender == 1, 1, 0)) |> 
  #Setting values for Race 
  #(Non-Hispanic White = 0, Mexican American = 1, Other Hispanic  = 2, Non-Hispanic Black = 3,
  #Non-Hispanic Asian = 4, Other Race - Including Multi-Racial = 5)
  mutate(Race = case_when(Race == 1 ~ 1,
                          Race == 2 ~ 2,
                          Race == 3 ~ 0,
                          Race == 4 ~ 3,
                          Race == 6 ~ 4,
                          Race == 7 ~ 5)
  ) |> 
  
  #Depression data cleaning
  
  #Calculating PHQ-9 Score
  mutate(across(c(DPQ010:DPQ090), ~ replace_values(.x, depression_refused_dunno ~ NA))) |> 
  mutate(PHQ9_Score = rowSums(across(DPQ010:DPQ090), na.rm = FALSE)) |> 
  #Categorizing depression yes/no (score >= 10)
  mutate(depression = if_else(PHQ9_Score >= 10, 1, 0)) |> 
  
  #Physical activity data cleaning
  
  #Setting factor variables as factors
  mutate(PAD790U = as.factor(PAD790U),
         PAD810U = as.factor(PAD810U)) |> 
  #Setting "refused", "dont know" or "0" as missing
  mutate(across(c(PAD790Q, PAD800, PAD810Q, PAD820, PAD680), ~ replace_values(.x, pa_refused_or_dunno ~ NA))) |> 

  #Renaming sitting time
  rename("sitting_time" = "PAD680") |>
  #Transforming observation data to minutes_per_week of moderate activity
  mutate(moderate_weekly_minutes = case_when(
    PAD790U == "D" ~ PAD800 * PAD790Q * 7,
    PAD790U == "M" ~ (PAD800 * PAD790Q) / 4,
    PAD790U == "Y" ~ (PAD800 * PAD790Q) / 52,
    PAD790U == "W" ~ PAD800 * PAD790Q)
  ) |> 
  mutate(moderate_weekly_minutes = round(moderate_weekly_minutes, digits = 0)) |> 
  #calculating weekly MET-minutes
  mutate(moderate_met = moderate_weekly_minutes * 5) |> 
  #Transforming observation data to minutes_per_week of vigorous activity
  mutate(vigorous_weekly_minutes = case_when(
    PAD810U == "D" ~ PAD820 * PAD810Q * 7,
    PAD810U == "M" ~ (PAD820 * PAD810Q) / 4,
    PAD810U == "Y" ~ (PAD820 * PAD810Q) / 52,
    PAD810U == "W" ~ PAD820 * PAD810Q )) |> 
  mutate(vigorous_weekly_minutes = round(vigorous_weekly_minutes, digits = 0)) |>
  #Calculating weekly MET-minutes
  mutate(vigorous_met = vigorous_weekly_minutes * 7) |>
  #Summing up MET (vigorous + moderate)
  mutate(weekly_MET = rowSums(pick(moderate_met, vigorous_met), na.rm = TRUE)) |>
  #Summing moderate (* 1) and vigorous (* 2) for WHO guideline equivalency
  mutate(eq_vigorous_weekly_minutes = vigorous_weekly_minutes * 2) |> 
  mutate(who_guideline_total = rowSums(pick(moderate_weekly_minutes, eq_vigorous_weekly_minutes), na.rm = TRUE)
  ) |> 
  #WHO guideline (150 moderate minutes or 75 vigorous minutes or equivalent; yes or no
  mutate(who_guideline = if_else(who_guideline_total >= 150, 1, 0)) |> 
  
  #Smoking data cleaning
  
  #Setting "refused" and "dont know" to NA
  mutate(across(c(SMQ020, SMQ040), ~ replace_values(.x, smoking_refused_or_dunno ~ NA))) |> 
  #
  mutate(smoker = if_else(SMQ020 == 1 &
                            SMQ040 %in% c(1, 2),
                          1, 0)
  ) |> 
  
  #Alcohol use data cleaning
  
  #Setting "refused" and "dont know" to NA
  mutate(across(ALQ111, ~ replace_values(.x, alcohol_small_refused_or_dunno ~ NA))) |> 
  mutate(across(ALQ121, ~ replace_values(.x, alcohol_medium_refused_or_dunno ~ NA))) |> 
  mutate(across(ALQ130, ~ replace_values(.x, alcohol_large_refused_or_dunno ~ NA))) |> 
  #Setting heavy drinker = 2, drinker = 1, non-drinker = 0
  mutate(alcohol_use = case_when(ALQ111 == 1 & Gender == 1 & ALQ130 >= 2 ~ 2,
                                 ALQ111 == 1 & Gender == 1 & ALQ130 == 1 ~ 1,
                                 ALQ111 == 1 & Gender == 0 & ALQ130 >= 1 ~ 2,
                                 ALQ111 == 2 & ALQ121 == 0 ~ 0,
                                 ALQ111 == 2 ~ 0,
                                 .default = 0
                                 )
  ) |> 
  
  
  #Unemployment data cleaning
  
  #Setting OCD150 and OCQ383 "refused" and "dont know" to NA
  mutate(across(OCD150, ~ replace_values(.x, OCD150_refused_or_dunno ~ NA))) |>
  mutate(across(OCQ383, ~ replace_values(.x, OCQ383_refused_or_dunno ~ NA))) |> 
  #Setting employed subjects as 0 and unemployed as 1
  mutate(unemployed = case_when(OCQ383 %in% c(5, 6, 7) ~ 1,
                                OCD150 == 3 ~ 1,
                                .default = 0)
  ) |>
  
  #Diabetes data cleaning
  
  #Setting "refused" and "dont know" to NA
  mutate(across(c(DIQ010, DIQ160, DIQ050, DIQ070), ~ replace_values(.x, diabetes_refused_or_dunno ~ NA))) |> 
  #Changing "No" answer from "2" to "0"
  mutate(across(c(DIQ010, DIQ160, DIQ050, DIQ070), ~ replace_values(.x, 2 ~ 0))) |> 
  #Categorizing diabetess (yes = 1, no = 0) based on if they answered YES to ANY of the questions
  mutate(diabetes = if_else(
    if_any(c(DIQ010, DIQ160, DIQ050, DIQ070), ~ . == 1), 1, 0, missing = 0)) |>
  
  #Blood pressure data cleaning
  
  #Setting "refused" and "dont know" to NA
  mutate(across(BPQ150, ~ replace_values(.x, bpcholesterol_refused_or_dunno ~ NA))) |> 
  #Average systolic and diastolic
  mutate(avg_systolic_bp = round(rowMeans(pick(BPXOSY1, BPXOSY2, BPXOSY3))),
         avg_diastolic_bp = round(rowMeans(pick(BPXODI1, BPXODI2, BPXODI3)))
         ) |> 
  #Defining hypertension
  mutate(hypertension = case_when(avg_systolic_bp >= 130 ~ 1,
                                  avg_diastolic_bp >= 80 ~ 1,
                                  BPQ030 == 1 ~ 1,
                                  BPQ150 == 1 ~ 1,
                                  .default = 0)
  ) |> 
  
  #Cholesterol data cleaning
  
  #Setting "refused" and "dont know" to NA
  mutate(across(c(BPQ080, BPQ101D), ~ replace_values(.x, bpcholesterol_refused_or_dunno ~ NA))) |>
  #Defining dyslipidemia
  mutate(dyslipidemia = if_else(BPQ080 == 1 |
                                  BPQ101D == 1,
                                1, 0)) |> 
  
  #Comorbidities data cleaning
  
  #Setting "refused" and "dont know" to NA
  mutate(across(
    c(MCQ010, MCQ035, MCQ160A, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F, MCQ160M, MCQ170M, MCQ160P, MCQ160L, MCQ170L, MCQ220),
    ~ replace_values(.x, comorbidities_refused_or_dunno ~ NA))) |> 
  #Defining other comorbidities
  mutate(asthma = if_else(MCQ010 == 1 & MCQ035 == 1, 1, 0),
         arthritis = if_else(MCQ160A == 1, 1, 0),
         chf = if_else(MCQ160B == 1, 1, 0),
         chd = if_else(MCQ160C == 1, 1, 0),
         angina = if_else(MCQ160D == 1, 1, 0),
         heart_attack = if_else(MCQ160E == 1, 1, 0),
         stroke = if_else(MCQ160F == 1, 1, 0),
         thyroid = if_else(MCQ160M == 1 & MCQ170M == 1, 1, 0),
         copd = if_else(MCQ160P == 1, 1, 0),
         liver_disease = if_else(MCQ160L == 1 & MCQ170L == 1, 1, 0),
         cancer = if_else(MCQ220 == 1, 1, 0)
  ) |> 
  #Defining total comorbidity burden (0, 1 or >=2 comorbidities)
  mutate(comorbidity_total = rowSums(
    pick(
      diabetes, hypertension, dyslipidemia, asthma, arthritis, chf, chd, angina, heart_attack, stroke, thyroid, copd, liver_disease, cancer), na.rm = TRUE)
  ) |> 
  mutate(comorbidity_burden = if_else(comorbidity_total >= 2, 2, comorbidity_total)
  ) |> 


  #Setting main sample

  mutate(main_sample = if_else(
    if_any(DPQ010:DPQ090, is.na) |
      if_all(c(PAD790Q, PAD800, PAD810Q, PAD820), is.na) |
      is.na(sitting_time) |
      if_all(c(SMQ020, SMQ040), is.na) |
      is.na(ALQ111) |
      if_all(c(ALQ111, ALQ121, ALQ130), is.na) |
      if_all(c(diabetes, hypertension, dyslipidemia, asthma, arthritis, chf, chd, angina, heart_attack, stroke, thyroid, copd, liver_disease, cancer), is.na) |
      is.na(OCD150) |
      (OCD150 %in% c(4) & is.na(OCQ383)),
    0, 1)) |> 

  #Removing unnecessary variables

  select(!c(DPQ010:DPQ090)) |>
  select(!starts_with("PAD")) |> 
  select(!c(SMQ020, SMQ040)) |> 
  select(!starts_with("ALQ")) |> 
  select(!c(DIQ010, DIQ160, DIQ050, DIQ070)) |>
  select(!c(BPXOSY1, BPXOSY2, BPXOSY3, BPXODI1, BPXODI2, BPXODI3, 
            avg_systolic_bp, avg_diastolic_bp, BPQ020, BPQ030, BPQ150)) |> 
  select(!c(BPQ080, BPQ101D)) |> 
  select(!starts_with("MCQ")) |> 
  select(!c(OCD150, OCQ383))

view(nhanes_project_2)