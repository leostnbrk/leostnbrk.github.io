rm(list = ls())

library(tidyverse)

source("functions_plot.R")
source("functions_models.R")
source("functions_simulation.R")

# ================= DEFINE SIMULATION ================= 

simulation  <- list(
  model = "baseline_SIS",
  replications = 15,
  tbl_parameter = expand.grid(
    transmit_prob = 1:9/10
  ),
  master_seed  = 42,
  max_cores    = 15
)

results <- run_simulation(simulation)

