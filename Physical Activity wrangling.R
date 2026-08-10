#Physical Activity

library(tidyverse)

#Loading raw data

pa_sitting_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/PAQ_L.xpt")

#Defining "refused", "dont know" and "0"

pa_refused_dunno_or_zero <- c(0, 7777, 9999)

#Cleaning the data

pa_sitting_clean <- pa_sitting_raw |> 
  #Setting factor variables as factors
  mutate(PAD790U = as.factor(PAD790U),
         PAD810U = as.factor(PAD810U)) |> 
  #Setting "refused", "dont know" or "0" as missing
  mutate(across(!c(SEQN, PAD790U, PAD810U), ~ replace_values(.x, pa_refused_dunno_or_zero ~ NA))) |> 
  #Removing observations with all values of physical activity variables as missing
  filter(!if_all(c(PAD790Q, PAD800, PAD810Q, PAD820), is.na)) |> 
  #Removing observations with missing sitting time
  filter(!is.na(PAD680)) |> 
  #Renaming sitting time
  rename("sitting_time" = "PAD680") |> 
  #Transforming observation data to minutes_per_week of moderate activity
  group_by(SEQN) |> 
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
  select(!starts_with("PAD"))

view(pa_sitting_clean)