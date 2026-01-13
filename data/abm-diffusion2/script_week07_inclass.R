rm(list = ls())

library(tidyverse)

source("functions_plot.R")
source("functions_models.R")
source("functions_simulation.R")

# ================= DEFINE SIMULATION ================= 

simulation  <- list(
  model = "week07_inclass",
  replications = 15,
  tbl_parameter = expand.grid(
    init_R = seq(0, .9, length.out = 10),
    init_I = 5
  ),
  master_seed  = 42,
  max_cores    = 15
)

results <- run_simulation(simulation)
saveRDS(results, file = "week07_inclass_01.rds")

plot_avg_type(results$df_time_series, show = c("I"), group_var = init_R)

results$df_time_series |> filter(step == 200) |> pull(I_share_mean, init_R)

results$df_time_series |> 
  filter(step == 100) |> 
  mutate(
    lower = I_share_mean - qnorm(0.975) * I_share_sd / sqrt(replications),
    upper = I_share_mean + qnorm(0.975) * I_share_sd / sqrt(replications)
  ) |> 
  ggplot( aes(x = init_R, y = I_share_mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  geom_point() +
  labs(
    x = "Initial removal share (init_R)",
    y = "Mean infected share at step 100",
    title = "Infection prevalence at step 100 with 95% CI"
  ) +
  theme_minimal()
