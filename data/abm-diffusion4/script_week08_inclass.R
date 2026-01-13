rm(list = ls())

library(tidyverse)
library(lhs)
library(sensitivity)
library(igraph)
library(plotly)

library(gganimate)

source("functions_plot.R")
source("functions_models.R")
source("functions_simulation.R")

### Grid search (Brute Force)

simulation  <- list(
  model = "all0",
  replications = 15,
  tbl_parameter = expand.grid(
    network = "pa",
    network_m = 5,                        #  the number of edges to add in each time step
    transmit_prob = seq(.01, .12, by=.01),
    I_S_prob      = 0.14
  ),
  master_seed  = 42,
  max_cores    = 15
)

# run the simulation
#results <- run_simulation(simulation)

# save space and drop unnecessary data
#results$df_time_series   <- NULL
#saveRDS(results, file = "week08_inclass_results_01.rds")
results <- readRDS(file = "week08_inclass_results_01.rds")

df_final_worlds <- results$df_final_worlds

df_final_worlds <- df_final_worlds |> 
  mutate(
    degree = sapply(neighbors, length)
  )

table(df_final_worlds$days_infected)
ggplot(df_final_worlds, aes(x = days_infected)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black")

df_anim <- df_final_worlds |>
  mutate(transmit_prob_label = paste0("Transmit prob: ", transmit_prob))

# Create the animated plot
p <- ggplot(df_anim, aes(x = days_infected)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "{closest_state}", x = "Days Infected", y = "Count") +
  transition_states(transmit_prob_label, transition_length = 0, state_length = 1) +
  ease_aes("linear") +
  theme_minimal()

# Render the animation (this opens a preview or saves it)
animate(p, width = 600, height = 400, fps = 5, duration = 10, renderer = gifski_renderer("hist_animation.gif"))


table(df_final_worlds$degree)
mean(df_final_worlds$degree)

ggplot(df_final_worlds, aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black")

df_final_worlds <- df_final_worlds |> 
  mutate(
    transmit_prob100 = transmit_prob * 100,
    degree_simple    = if_else( degree > 21, 21, degree)
  )

summary(lm(days_infected ~ degree,  df_final_worlds))
summary(lm(days_infected ~ degree * transmit_prob100,  df_final_worlds))

df_summary <- df_final_worlds |> 
  group_by(degree_simple, transmit_prob100) |> 
  summarise(
    days_infected_mean = mean(days_infected),
    .groups = "drop"  # optional, for cleaner output
  )

plot_heatmap (df       = df_summary, 
              x_var    = "transmit_prob100", 
              y_var    = "degree_simple",
              fill_var = "days_infected_mean",
              min_mid_max = c(0, max(df_summary$days_infected_mean)/2, max(df_summary$days_infected_mean)))


