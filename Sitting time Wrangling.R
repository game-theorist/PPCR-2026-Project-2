#Sitting time

#Loading raw data

pa_raw <- read_xpt("G:/My Drive/PPCR/Project 2/NHANES Data/PAQ_L.xpt")

#Cleaning

refused_or_dunno <- function(x) {
  x %in% c(7777, 9999)
}

sitting_clean <- pa_raw |> 
  select(SEQN, PAD680) |> 
  drop_na() |> 
  filter_out(
    if_any(starts_with("PAD"), refused_or_dunno)
  ) |> 
  rename("sitting_time" = "PAD680")

glimpse(sitting_clean)


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