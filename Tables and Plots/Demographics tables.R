library(tidyverse)
library(gt)

#Demmographics tables

#Age

nhanes_project_2 |> 
  summarize(missing = sum(is.na(Age)),
            proportion_missing = round(mean((is.na(Age))), digits = 2),
            available_data = sum(!is.na(Age)),
            median = median(Age, na.rm = TRUE),
            iqr = IQR(Age, na.rm = TRUE)
  )|>
  gt() |> 
  tab_header(
    title = md("Age")
  ) |> 
  cols_label(
    missing = md("Missing<br>Data"),
    proportion_missing = md("Proportion <br>Missing <br>Data"),
    available_data = md("Available <br>Data"),
    median = md("Median"),
    iqr = md("Inter- <br>Quartile <br>Range")
  ) 

#Gender

nhanes_project_2 |> 
  summarize(missing = sum(is.na(Gender)),
            proportion_missing = round(mean((is.na(Gender))), digits = 2),
            available_data = sum(!is.na(Gender)),
            male = sum(Gender == 1, na.rm = TRUE),
            female = sum(Gender == 0, na.rm = TRUE),
            male_prop = round(mean(Gender == 1, na.rm = TRUE), digits = 3),
            female_prop = round(mean(Gender == 0, na.rm = TRUE), digits = 3)
            
  ) |> 
  gt() |> 
  tab_header(
    title = md("Gender")
  ) |> 
  fmt_percent(
    columns = c(male_prop, female_prop),
    decimals = 2
  ) |> 
  cols_merge_n_pct(
    col_n = male,
    col_pct = male_prop
  ) |> 
  cols_merge_n_pct(
    col_n = female,
    col_pct = female_prop
  ) |> 
  cols_label(
    missing = md("Missing<br>Data"),
    proportion_missing = md("Proportion <br>Missing <br>Data"),
    available_data = md("Available <br>Data"),
    male = md("Male"),
    female = md("Female")
  )

#Race and Ethnicity

#Values for Race 
#(Non-Hispanic White = 0, Mexican American = 1, Other Hispanic  = 2, Non-Hispanic Black = 3,
#Non-Hispanic Asian = 4, Other Race - Including Multi-Racial = 5)


nhanes_project_2 |> 
  group_by(Race) |> 
  summarize(n = n()) |> 
  mutate(proportion =  n / sum(n)) |> 
  pivot_wider(
    names_from = Race,
    values_from = c(n, proportion)
  ) |> 
  
  gt() |> 
  tab_header(
    title = md("Race and Ethnicity")
  ) |> 
  fmt_percent(
    columns = starts_with("proportion"),
    decimals = 1
  ) |> 
  cols_merge_n_pct(
    col_n = n_0,
    col_pct = proportion_0
  ) |> 
  cols_merge_n_pct(
    col_n = n_1,
    col_pct = proportion_1
  ) |> 
  cols_merge_n_pct(
    col_n = n_2,
    col_pct = proportion_2
  ) |> 
  cols_merge_n_pct(
    col_n = n_3,
    col_pct = proportion_3
  ) |> 
  cols_merge_n_pct(
    col_n = n_4,
    col_pct = proportion_4
  ) |> 
  cols_merge_n_pct(
    col_n = n_5,
    col_pct = proportion_5
  ) |> 
  cols_label(
    n_0 = md("Non-Hispanic White"),
    n_1 = md("Mexican American"),
    n_2 = md("Other Hispanic"),
    n_3 = md("Non-Hispanic Black"),
    n_4 = md("Non-Hispanic Asian"),
    n_5 = md("Other Race")
      )
    
    # Ratio income poverty
    
    nhanes_project_2 |> 
      summarize(missing = sum(is.na(Ratio_income_poverty)),
                proportion_missing = round(mean((is.na(Ratio_income_poverty))), digits = 2),
                available_data = sum(!is.na(Ratio_income_poverty)),
                mean = round(mean(Ratio_income_poverty, na.rm = TRUE), digits = 2),
                sd = round(sd(Ratio_income_poverty, na.rm = TRUE), digits = 2),
                median = median(Ratio_income_poverty, na.rm = TRUE),
                iqr = IQR(Ratio_income_poverty, na.rm = TRUE)
      )|>
      gt() |> 
      tab_header(
        title = md("Ratio of family income to poverty")
      ) |> 
      cols_label(
        missing = md("Missing<br>Data"),
        proportion_missing = md("Proportion <br>Missing <br>Data"),
        available_data = md("Available <br>Data"),
        mean = md("Mean"),
        sd = md("Standard <br>Deviation"),
        median = md("Median"),
        iqr = md("Inter- <br>Quartile <br>Range")
      ) |> 
      tab_footnote(
        footnote = "Calculated by dividing total annual family (or individual) income by the poverty guidelines specific to the survey year",
        locations = cells_title()
      ) |> 
      tab_footnote(
        footnote = "Truncated by NHANES at 5.0",
        locations = cells_title()
      ) 
    
    