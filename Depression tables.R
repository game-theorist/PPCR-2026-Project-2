library(tidyverse)
library(gt)

#depression tables

#phq 9 score

nhanes_project_2 |> 
  summarize(row_label = "PHQ-9 Score",
            missing = sum(is.na(PHQ9_Score)),
            proportion_missing = round(mean((is.na(PHQ9_Score))), digits = 2),
            mean = round(mean(PHQ9_Score, na.rm = TRUE), digits = 2),
            sd = round(sd(PHQ9_Score, na.rm = TRUE), digits = 2),
            median = median(PHQ9_Score, na.rm = TRUE),
            iqr = IQR(PHQ9_Score, na.rm = TRUE)
            )|>
  gt(
    rowname_col = "row_label"
  ) |> 
  cols_label(
    missing = md("Missing<br>Values"),
    proportion_missing = md("Proportion <br>Missing <br>Values"),
    mean = md("Mean"),
    sd = md("Standard <br>Deviation"),
    median = md("Median"),
    iqr = md("Inter- <br>Quartile <br>Range")
  )

#depression categorized

nhanes_project_2 |> 
  summarize(row_label = "Depression",
            missing = sum(is.na(depression)),
            proportion_missing = round(mean((is.na(depression))), digits = 2),
            total = sum(depression, na.rm = TRUE),
            proportion = round(mean(depression, na.rm = TRUE), digits = 2)
            ) |>
  gt(
    rowname_col = "row_label"
    ) |> 
  cols_label(
    missing = md("Missing<br>Values"),
    proportion_missing = md("Proportion <br>Missing <br>Values"),
    total = md("Total Depressed"),
    proportion = md("Proportion Depressed")
  )