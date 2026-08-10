library(tidyverse)
library(haven)
library(psych)
library(skimr)

#Depression

#Loading raw data

dep_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/DPQ_L.xpt")

#PHQ-9 Score

dep_score <- dep_raw |>
  group_by(SEQN) |>
  filter(!if_any(c(DPQ010:DPQ090), ~ . %in% c(7,9))) |> 
  drop_na(!DPQ100) |> 
  summarize(
    PHQ9_Score = rowSums(across(DPQ010:DPQ090), na.rm = TRUE)
  )

#Depression yes/no (Score >= 10)

dep_cat_score <- dep_score |> 
  mutate(Depression = if_else(PHQ9_Score >= 10, 1, 2))

View(dep_cat_score)

#Plots

ggplot(dep_cat_score, aes(x = PHQ9_Score)) +
  geom_histogram(binwidth = 1)

ggplot(dep_cat_score, aes(y = PHQ9_Score, x = Depression)) +
  geom_boxplot()

prop_depressed <- mean(dep_cat_score$Depression == "1")

