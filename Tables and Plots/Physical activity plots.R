#moderate weekly minutes

nhanes_project_2 |> 
  ggplot(aes(x = moderate_weekly_minutes)) +
  geom_histogram(binwidth = 50, boundary = 0, fill = "lightblue") +
  scale_x_continuous(limits = c(0, 4000), expand = c(0.01, 0.01)) +
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
  ggplot(aes(x = moderate_weekly_minutes)) +
  geom_density(fill = "lightblue", color = "lightblue") +
  scale_x_continuous(limits = c(0, 4000), expand = c(0.01, 0.01)) +
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
#vigorous weekly minutes

nhanes_project_2 |> 
  ggplot(aes(x = vigorous_weekly_minutes)) +
  geom_histogram(binwidth = 50, boundary = 0, fill = "lightblue") +
  scale_x_continuous(limits = c(0, 4000), expand = c(0.01, 0.01)) +
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
  ggplot(aes(x = vigorous_weekly_minutes)) +
  geom_density(fill = "lightblue", color = "lightblue") +
  scale_x_continuous(limits = c(0, 4000), expand = c(0.01, 0.01)) +
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
