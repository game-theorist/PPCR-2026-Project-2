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

# 1 Mexican American	2	Other Hispanic 3	Non-Hispanic White 4	Non-Hispanic Black 6	Non-Hispanic Asian 7	Other Race - Including Multi-Racial


nhanes_project_2 |> 
  summarize(missing = sum(is.na(Race)),
            proportion_missing = round(mean((is.na(Race))), digits = 2),
            available_data = sum(!is.na(Race)),
            mexican_american = sum(Race == 1, na.rm = TRUE),
            mexican_american_prop = round(mean(Race == 1, na.rm = TRUE), digits = 3),
            other_hispanic = sum(Race == 2, na.rm = TRUE),
            other_hispanic_prop = round(mean(Race == 2, na.rm = TRUE), digits = 3),
            non_hispanic_white = sum(Race == 3, na.rm = TRUE),
            non_hispanic_white_prop = round(mean(Race == 3, na.rm = TRUE), digits = 3),
            non_hispanic_black = sum(Race == 4, na.rm = TRUE),
            non_hispanic_black_prop = round(mean(Race == 4, na.rm = TRUE), digits = 3),
            non_hispanic_asian = sum(Race == 6, na.rm = TRUE),
            non_hispanic_asian_prop = round(mean(Race == 6, na.rm = TRUE), digits = 3)
            ) |> 
  gt() |> 
  tab_header(
    title = md("Race and Ethnicity")
  ) |> 
  fmt_percent(
    columns = c(mexican_american_prop, other_hispanic_prop, non_hispanic_white_prop, non_hispanic_black_prop, non_hispanic_asian_prop),
    decimals = 1
  ) |> 
  cols_merge_n_pct(
    col_n = mexican_american,
    col_pct = mexican_american_prop
  ) |> 
  cols_merge_n_pct(
    col_n = other_hispanic,
    col_pct = other_hispanic_prop
  ) |> 
  cols_merge_n_pct(
    col_n = non_hispanic_white,
    col_pct = non_hispanic_white_prop
  ) |> 
  cols_merge_n_pct(
    col_n = non_hispanic_black,
    col_pct = non_hispanic_black_prop
  ) |> 
  cols_merge_n_pct(
    col_n = non_hispanic_asian,
    col_pct = non_hispanic_asian_prop
  ) |>
  cols_label(
    missing = md("Missing<br>Data"),
    proportion_missing = md("Proportion <br>Missing <br>Data"),
    available_data = md("Available <br>Data"),
    mexican_american = md("Mexican American"),
    other_hispanic = md("Other Hispanic"),
    non_hispanic_white = md("Non-Hispanic White"),
    non_hispanic_black = md("Non-Hispanic Black"),
    non_hispanic_asian = md("Non-Hispanic Asian")
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

