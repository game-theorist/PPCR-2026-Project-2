#Smoking status

library(tidyverse)
library(haven)

#Loading raw data

smoking_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/SMQ_L.xpt")

#Defining "refused" or "dont know"
smoking_refused_or_dunno <- c(7, 9)

#Cleaning the data

smoking_clean <- smoking_raw |> 
  #Selecting ID and "do you smoke now?"
  select(SEQN, SMQ040) |> 
  #Setting "refused" and "dont know" to NA
  mutate(across(!c(SEQN), ~ replace_values(.x, smoking_refused_or_dunno ~ NA))) |> 
  #Replacing 2 for "No" with 0
  group_by(SEQN) |> 
  summarize(smoker = if_else(SMQ040 == 2, 0, 1)) |> 
  #Removing missing values
  drop_na()

view(smoking_clean)