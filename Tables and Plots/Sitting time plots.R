library(tidyverse)
library(patchwork)

#sitting time plots

nhanes_project_2 |> 
  ggplot(aes(x = sitting_time)) +
  geom_histogram(binwidth = 120, boundary = 0, fill = "lightgreen") +
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

nhanes_project_2 |> 
  ggplot(aes(x = sitting_time)) +
  geom_density(fill = "lightgreen", color = "lightgreen") +
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