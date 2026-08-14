library(tidyverse)
library(gt)

#WHO guideline total

nhanes_project_2 |> 
  summarize(who_missing = sum(if_all(c(moderate_weekly_minutes, vigorous_weekly_minutes), is.na)),
            who_proportion_missing = round(mean((if_all(c(moderate_weekly_minutes, vigorous_weekly_minutes), is.na))), digits = 2),
            who_available_data = sum(!if_all(c(moderate_weekly_minutes, vigorous_weekly_minutes), is.na)),
            across(who_guideline_total,
                   list(mean = ~ round(mean(.x, na.rm = TRUE), digits = 2),
                        sd = ~ round(sd(.x, na.rm = TRUE), digits = 2),
                        median = ~ round(median(.x, na.rm = TRUE), digits = 2),
                        iqr = ~ IQR(.x, na.rm = TRUE))),
            who_guideline_met = sum(who_guideline, na.rm = TRUE),
            who_guideline_met_proportion = round(mean(who_guideline, na.rm = TRUE), digits = 2)
            
  ) |> 
  gt() |> 
  tab_header(
    title = md("Minutes of Activity per Week")
  ) |> 
  cols_label(ends_with("missing") ~ md("Missing<br>Data"),
             ends_with("proportion_missing") ~ md("Proportion <br>Missing <br>Data"),
             ends_with("available_data") ~md("Available <br>Data"),
             ends_with("mean") ~ md("Mean"),
             ends_with("sd") ~ md("Standard <br>Deviation"),
             ends_with("median") ~ md("Median"),
             ends_with("iqr") ~ md("Inter- <br>Quartile <br>Range"),
             who_guideline_met ~ md("Guideline <br>Met"),
             who_guideline_met_proportion ~ md("Proportion <br>Guideline <br>Met")
  ) |>  
  tab_spanner(label = md("**WHO <br>Guideline <br>Equivalent**"),
              columns = starts_with("who")
  ) |> 
  tab_footnote(
    footnote = "Total equivalent equals minutes per week of moderate activity + vigorous activity x 2 ",
    locations = cells_column_spanners()
  ) |> 
  tab_footnote(
    footnote = "Participants with total equivalent >= 150 minutes",
    locations = cells_column_labels(
      columns = who_guideline_met
    )
  )
