library(tidyverse)
library(gt)

#depression tables

#phq 9 score

nhanes_project_2 |> 
  summarize(missing = sum(is.na(PHQ9_Score)),
            proportion_missing = round(mean((is.na(PHQ9_Score))), digits = 2),
            available_data = sum(!is.na(PHQ9_Score)),
            mean = round(mean(PHQ9_Score, na.rm = TRUE), digits = 2),
            sd = round(sd(PHQ9_Score, na.rm = TRUE), digits = 2),
            median = median(PHQ9_Score, na.rm = TRUE),
            iqr = IQR(PHQ9_Score, na.rm = TRUE),
            depression = sum(depression, na.rm = TRUE)
            )|>
  gt() |> 
  tab_header(
    title = md("PHQ-9 Score")
  ) |> 
  cols_label(
    missing = md("Missing<br>Data"),
    proportion_missing = md("Proportion <br>Missing <br>Data"),
    available_data = md("Available <br>Data"),
    mean = md("Mean"),
    sd = md("Standard <br>Deviation"),
    median = md("Median"),
    iqr = md("Inter- <br>Quartile <br>Range"),
    depression = md("Depression")
  ) |> 
  tab_footnote(
    footnote = "Defined as PHQ-9 Score >= 10",
    locations = cells_column_labels(
      columns = depression
    )
  ) 