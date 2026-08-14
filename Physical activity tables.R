library(tidyverse)
library(gt)

#PA tables

nhanes_project_2 |> 
  summarize(across(c(moderate_weekly_minutes, vigorous_weekly_minutes),
                   list(missing = ~ sum(is.na(.x)),
                        proportion_missing = ~ round(mean(is.na(.x)), digits = 2),
                        mean = ~ round(mean(.x, na.rm = TRUE), digits = 2),
                        sd = ~ round(sd(.x, na.rm = TRUE), digits = 2),
                        median = ~ round(median(.x, na.rm = TRUE), digits = 2),
                        iqr = ~ IQR(.x, na.rm = TRUE)))
  ) |> 
  gt() |> 
  tab_header(
    title = md("Minutes of Activity per Week")
  ) |> 
  cols_label(ends_with("missing") ~ md("Missing<br>Values"),
             ends_with("proportion_missing") ~ md("Proportion <br>Missing <br>Values"),
             ends_with("mean") ~ md("Mean"),
             ends_with("sd") ~ md("Standard <br>Deviation"),
             ends_with("median") ~ md("Median"),
             ends_with("iqr") ~ md("Inter- <br>Quartile <br>Range")
  ) |> 
  tab_spanner(label = md("**Moderate**"),
              columns = starts_with("moderate")
  ) |> 
  tab_spanner(label = md("**Vigorous**"),
              columns = starts_with("vigorous")
  )

#WHO guideline

nhanes_project_2 |> 
  summarize(missing = sum(all(if_any(c(moderate_weekly_minutes, vigorous_weekly_minutes), is.na),
                              moderate_weekly_minutes < 150,
                              vigorous_weekly_minutes < 75)),
                        across(who_guideline_total,
                               list(mean = ~ round(mean(.x, na.rm = TRUE), digits = 2),
                                    sd = ~ round(sd(.x, na.rm = TRUE), digits = 2),
                                    median = ~ round(median(.x, na.rm = TRUE), digits = 2),
                                    iqr = ~ IQR(.x, na.rm = TRUE)))
  ) |> 
  gt() |> 
  tab_header(
    title = md("Minutes of Activity per Week")
  ) |> 
  cols_label(ends_with("mean") ~ md("Mean"),
             ends_with("sd") ~ md("Standard <br>Deviation"),
             ends_with("median") ~ md("Median"),
             ends_with("iqr") ~ md("Inter- <br>Quartile <br>Range")
  ) |>  
  tab_spanner(label = md("**WHO <br>Guideline <br>Equivalent**"),
              columns = starts_with("who")
  ) |> 
  tab_footnote(
    footnote = "Total equivalent equals minutes per week of moderate activity + vigorous activity x 2 ",
    locations = cells_column_spanners()
  )
