#Plots

#depression

#PHQ-9 Score

PHQ9_summary <- nhanes_project_2 |> 
  summarize(mean = mean(PHQ9_Score, na.rm = TRUE),
            sd = sd(PHQ9_Score, na.rm = TRUE),
            median = median(PHQ9_Score, na.rm = TRUE),
            iqr = IQR(PHQ9_Score, na.rm = TRUE))

phq9_density <- nhanes_project_2 |> 
  ggplot(aes(x = PHQ9_Score)) +
  geom_density(fill = "orange", color = "orange") +
  scale_x_continuous(limits = c(0, NA), expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  xlab("PHQ-9 Score") +
  ylab(NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 17)),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )

phq9_histogram <- nhanes_project_2 |> 
  ggplot(aes(x = PHQ9_Score)) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "orange") +
  scale_x_continuous(limits = c(0, NA), expand = c(0.33333333333333333333333333333301, 0.01)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  xlab("PHQ-9 Score") +
  ylab(NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 17)),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )

phq9_histogram + phq9_density + plot_layout(axes = "collect")