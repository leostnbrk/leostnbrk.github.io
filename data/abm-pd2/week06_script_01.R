rm(list = ls())

library(tidyverse)

source("functions_plot.R")
source("functions_sequence.R")
source("functions_simulation.R")
source("functions_models.R")

simulation  <- list(
  model = "baseline_very_long",
  replications = 15,
  tbl_parameter = expand.grid(
    mutators_n_fixed = c(TRUE, FALSE),
    mu_rnd = c(0.2, 0.1, 0.05, 0.01, 0.001)
  ),
  master_seed  = 42,
  max_cores    = 15
)

results <- run_simulation(simulation)

saveRDS(results, "week06_results_01.rds", compress = "xz")   

plot_avg_fitness(results$df_population %>% filter(mutators_n_fixed == TRUE),
                 mu_rnd,
                 simulation$replications)


plot_avg_fitness(results$df_population %>% filter(mutators_n_fixed == FALSE),
                 mu_rnd,
                 simulation$replications)