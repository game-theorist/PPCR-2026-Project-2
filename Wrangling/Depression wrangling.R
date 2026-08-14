library(tidyverse)
library(haven)
library(psych)
library(skimr)

#Depression

#Loading raw data

depression_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DPQ_L.xpt")

#Defining "refused" and "dont know"

depression_refused_dunno <- c(7, 9)

#Cleaning the data

depression_clean <- depression_raw |>
  #Calculating PHQ-9 Score
  mutate(across(c(DPQ010:DPQ090), ~ replace_values(.x, depression_refused_dunno ~ NA))) |> 
  filter(!if_any(c(DPQ010:DPQ090), is.na))|> 
  group_by(SEQN) |> 
  summarize(
    PHQ9_Score = rowSums(across(DPQ010:DPQ090), na.rm = TRUE)
  ) |> 
  #Categorizing depression yes/no (score >= 10)
  mutate(Depression = if_else(PHQ9_Score >= 10, 1, 2))

view(depression_clean)

#Plots

ggplot(dep_cat_score, aes(x = PHQ9_Score)) +
  geom_histogram(binwidth = 1)

ggplot(dep_cat_score, aes(y = PHQ9_Score, x = Depression)) +
  geom_boxplot()

prop_depressed <- mean(dep_cat_score$Depression == "1")