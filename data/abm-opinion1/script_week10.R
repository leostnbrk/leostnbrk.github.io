rm(list = ls())

library(tidyverse)
library(lhs)
library(sensitivity)
library(igraph)
library(plotly)
library(gganimate)
library(igraphdata)

source("functions_plot.R")
source("functions_models.R")
source("functions_simulation.R")
source("functions_sequence.R")
source("functions_calibration.R")

simulation  <- list(
  model = "karate",
  replications = 1,
  tbl_parameter = expand.grid(
    influence_epsilon = seq(0.40, 1, by=.2)
  ),
  master_seed  = 42
)

data("karate")

results <- run_simulation(simulation,
                          empirical_data = V(karate)$Faction
                          )

saveRDS(results, file = "week11_results.rds")

results$df_calibration
