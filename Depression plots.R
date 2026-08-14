library(tidyverse)
library(patchwork)

#Plots

#depression

#PHQ-9 Score

phq9_density <- nhanes_project_2 |> 
  ggplot(aes(x = PHQ9_Score)) +
  geom_density(fill = "orange", color = "orange") +
  scale_x_continuous(limits = c(0, NA), expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(NULL) +
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
  scale_x_continuous(limits = c(0, NA), expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 17)),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )

# Depression categorized

cat_depression <- nhanes_project_2 |> 
  drop_na(depression) |> 
  ggplot(aes(x = as.factor(depression))) +
  geom_bar(fill = "orange") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 17)),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )


cat_depression
