library(readxl)

#Group 12 dataframe

nhanes_group_12_raw <- read_excel("G:/My Drive/PPCR/Project 2/Group12_Milestone4_Final_numeric.xlsx")

View(nhanes_group_12_raw)

#Adapting my dataframe

nhanes_project_2_adapted <- nhanes_project_2 |> 
  mutate(Gender = if_else(Gender == 0, 2, 1),
         seqn = SEQN,
         Depression = depression) |> 
  select(!c(SEQN, depression))

View(nhanes_project_2_adapted)

#Comparisons

nhanes_project_2_comparison <- nhanes_group_12_adapted |> 
  full_join(nhanes_project_2_adapted, by = "seqn") |> 
  filter(depression != Depression) |> 
  relocate(depression, Depression)

View (nhanes_project_2_comparison)
View (pa_sitting_raw)
