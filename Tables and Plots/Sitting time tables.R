library(tidyverse)
library(gt)

#sitting time tables

nhanes_project_2 |> 
  summarize(missing = sum(is.na(sitting_time)),
            proportion_missing = round(mean((is.na(sitting_time))), digits = 2),
            available_data = sum(!is.na(sitting_time)),
            mean = round(mean(sitting_time, na.rm = TRUE), digits = 2),
            sd = round(sd(sitting_time, na.rm = TRUE), digits = 2),
            median = median(sitting_time, na.rm = TRUE),
            iqr = IQR(sitting_time, na.rm = TRUE)
  )|>
  gt() |> 
  tab_header(
    title = md("Sitting time")
  ) |> 
  cols_label(
    missing = md("Missing<br>Data"),
    proportion_missing = md("Proportion <br>Missing <br>Data"),
    available_data = md("Available <br>Data"),
    mean = md("Mean"),
    sd = md("Standard <br>Deviation"),
    median = md("Median"),
    iqr = md("Inter- <br>Quartile <br>Range")
  )