#Unemployment

library(tidyverse)
library(haven)

#Loading raw data

unemployment_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/OCQ_L.xpt")

#Defining "refused" or "dont know"
OCD150_refused_or_dunno <- c(7, 9)
OCQ383_refused_or_dunno <- c(77, 99)

#Cleaning the data

unemployment_clean <- unemployment_raw |> 
  #Selecting ID, "Type of work done last week" and "Main reason did not work last week"
  select(SEQN, OCD150, OCQ383) |> 
  #Setting OCD150 and OCQ383 "refused" and "dont know" to NA
  mutate(across(OCD150, ~ replace_values(.x, OCD150_refused_or_dunno ~ NA))) |>
  mutate(across(OCQ383, ~ replace_values(.x, OCQ383_refused_or_dunno ~ NA))) |> 
  #Removing subjects with missing values in OCQ383 IF OCD150 = 4
  filter(!when_all(c(OCD150 = 4 & is.na(OCQ383)))) |> 
  #Setting employed subjects as 0 and unemployed as 1
  group_by(SEQN) |> 
  summarize(unemployed = case_when(OCQ383 %in% c(5, 6, 7) ~ 1,
                                   OCD150 == 3 ~ 1,
                                   .default = 0)
  )

view(unemployment_clean)