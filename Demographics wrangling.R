#Demographics

library(tidyverse)

#Loading raw data

demo_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DEMO_L.xpt")

#Cleaning the data

demographics_clean <- demo_raw |> 
  #Selecting variables of interest
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, WTINT2YR, INDFMPIR) |> 
  #Renaming variables and setting values for gender (Male = 1, Female = 0)
  group_by(SEQN) |> 
  summarize(Gender = if_else(RIAGENDR == 1, 1, 0),
         Age = RIDAGEYR,
         Race = RIDRETH3,
         Interview_Weight = WTINT2YR,
         Ratio_income_poverty = INDFMPIR 
         )

View(demographics_clean)