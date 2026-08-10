#Sitting time

#Loading raw data

#Sitting time summary statistics

sitting_clean_stats <- sitting_clean |> 
  summarize(
    mean_sitting_time = mean(sitting_time),
    sd_sitting_time = sd(sitting_time),
    median_sitting_time = median(sitting_time),
    iqr_sitting_time = IQR(sitting_time)
  )

sitting_clean_stats

#Plots

ggplot(sitting_clean, aes(x = sitting_time)) +
  geom_histogram(bins = 20)


ggplot(sitting_clean, aes(x = sitting_time)) +
  geom_density()