#Project analysis model diagnosis

library(qqplotr)
library(patchwork)

plot_residuals <- augmented_model |> 
  ggplot(aes(sample = .resid)) +
  stat_qq_point() +
  stat_qq_line(color = "red") +
  coord_flip() +
  theme_bw()

plot_scaled_residuals <- augmented_model |> 
  ggplot(aes(sample = .resid)) +
  stat_pp_point() +
  stat_pp_line(color = "red") +
  coord_flip() +
  theme_bw()

plot_residuals_versus_fitted <- augmented_model |> 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_jitter(width = 0, height = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

plot_residuals + plot_residuals_versus_fitted

augmented_model |> 
  ggplot(aes(x = .std.resid)) +
  geom_density()