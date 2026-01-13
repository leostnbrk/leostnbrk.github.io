rm(list = ls())

library(tidyverse)

source("functions_plot.R")
source("functions_simulation.R")
source("functions_models.R")

simulation  <- list(
  model = "with_error",
  replications = 15,
  tbl_parameter = expand.grid(
    error = seq(0, .2, length.out = 5)
  ),
  master_seed  = 42,
  max_cores    = 15
)

results <- run_simulation(simulation)

saveRDS(results, "week06_results_02.rds", compress = "xz")   

plot_avg_fitness(results$df_population,
                 error)

plot_avg_fitness(results$df_population %>% filter(mutators_n_fixed == TRUE),
                 error)
