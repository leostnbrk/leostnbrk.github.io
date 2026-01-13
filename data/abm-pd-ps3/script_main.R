rm(list = ls())

library(tidyverse)

source("functions_plot.R")
source("functions_sequence.R")
source("functions_simulation.R")
source("functions_models.R")

# ================= DEFINE SIMULATION ================= 

simulation  <- list(
  model = "problem_set03",
  replications = 15,
  tbl_parameter = expand.grid(
    mu = 0.05,
    error = 0.05,
    expected_rounds = list(c(10,10),
                           c(20,20),
                           c(40,40),
                           c(80,80))
  ),
  master_seed  = 1234,
  max_cores    = 15
)

# ================= RUN SIMULATION ================= 

results <- run_simulation(simulation)

saveRDS(results, "problem_set03_pd_results.rds", compress = "xz")   

results$df_population <- results$df_population |> 
  rowwise() |>
  mutate(expected_rounds_min = min(expected_rounds) )

results$df_final_worlds <- results$df_final_worlds |> 
  rowwise() |>
  mutate(expected_rounds_min = min(expected_rounds) )


plot_avg_fitness(results$df_population, group_var = expected_rounds_min)

