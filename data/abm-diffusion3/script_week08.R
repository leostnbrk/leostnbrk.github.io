rm(list = ls())

library(tidyverse)
library(lhs)
library(sensitivity)

source("functions_plot.R")
source("functions_models.R")
source("functions_simulation.R")

### Grid search (Brute Force)

simulation  <- list(
  model = "all0",
  replications = 15,
  tbl_parameter = expand.grid(
    transmit_prob = seq(0, 1, by=.1),
    I_R_prob      = seq(0, 1, by=.1),
    I_S_prob      = 0.5 
  ),
  master_seed  = 42,
  max_cores    = 15
)

results <- run_simulation(simulation)

# save space and drop unnecessary data
results$df_time_series   <- NULL
results$df_final_worlds  <- NULL

#saveRDS(results, file = "week08_results_01.rds")
results <- readRDS(results, file = "week08_results_01.rds")

# Create heatmap
plot_heatmap(df       = results$df_final_summary, 
             x_var    = "transmit_prob",
             y_var    = "I_R_prob",
             fill_var = "R_share_mean")

plot_SIR_color_blend(
  df = results$df_final_summary,
  x_var = "transmit_prob",
  y_var = "I_R_prob"
)


### Latin Hypercube Sampling (LHS)

parameters <- list(                              # provides random values in form of ...
  transmit_prob = list("qnumeric", 0.25, 0.60), # ... a numeric from the uniform distribution with values between 0.01 and 0.5
  I_R_prob      = list("qnumeric", 0.00, 1.00), # ... a numeric from the uniform distribution with values between 0.01 and 0.5
  I_S_prob      = list("qnumeric", 0.00, 1.00)  # ... a numeric from the normal distribution with mu = 0.125 and sd = 0.009
)

s <- 50
tbl_parameter <- tbl_parameters_LHS(parameters, s = 2 * s, seed = 123)

simulation  <- list(
  model = "all0",
  replications = 15,
  tbl_parameter = tbl_parameter,
  master_seed  = 42,
  max_cores    = 15
)

results <- run_simulation(simulation)

results$df_time_series   <- NULL
results$df_final_worlds  <- NULL

saveRDS(results, file = "week08_results_02.rds")
#results <- readRDS(file = "week08_results_02.rds")


tbl_parameter$y <- results$df_final_summary$R_share_mean

summary( lm(y ~ transmit_prob + I_R_prob + I_S_prob, data = tbl_parameter))

summary( lm(y ~ transmit_prob * I_R_prob * I_S_prob, data = tbl_parameter))


### Grid search (Brute Force) CHANGED ORDER OF 1A and 1C !!!!

simulation  <- list(
  model = "all0",
  replications = 15,
  tbl_parameter = expand.grid(
    transmit_prob = seq(0, 1, by=.1),
    I_R_prob      = seq(0, 1, by=.1),
    I_S_prob      = 0.5 
  ),
  master_seed  = 42,
  max_cores    = 15
)

results <- run_simulation(simulation)

# save space and drop unnecessary data
results$df_time_series   <- NULL
results$df_final_worlds  <- NULL

#saveRDS(results, file = "week08_results_03.rds")
results <- readRDS(file = "week08_results_03.rds")

plot_heatmap(df       = results3$df_final_summary, 
             x_var    = "transmit_prob",
             y_var    = "I_R_prob",
             fill_var = "R_share_mean")

plot_heatmap(df       = results1$df_final_summary, 
             x_var    = "transmit_prob",
             y_var    = "I_R_prob",
             fill_var = "R_share_mean")


plot_SIR_color_blend(
  df = results3$df_final_summary,
  x_var = "transmit_prob",
  y_var = "I_R_prob"
)

plot_SIR_color_blend(
  df = results1$df_final_summary,
  x_var = "transmit_prob",
  y_var = "I_R_prob"
)
