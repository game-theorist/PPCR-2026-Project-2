#Comorbidities plots

my_palette <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(20)

nhanes_project_2 |> 
  pivot_longer(
    cols = c(diabetes, hypertension, dyslipidemia, asthma, arthritis, chf, chd, angina, heart_attack, stroke, thyroid, copd, liver_disease, cancer),
    names_to = "Comorbidities",
    values_to = "value"
  ) |> 
  filter(value == 1) |> 
  ggplot(aes(y = fct_infreq(Comorbidities), fill = Comorbidities)) +
  geom_bar() +
  scale_fill_brewer(palette = "my_palette") +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 17)),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )
