rm(list = ls())

source("functions_plot.R")
source("functions_simulation.R")
source("functions_models.R")

# ================= DEFINE SIMULATION ================= 

simulation  <- list(
  model = "baseline",
  replications = 50,
  tbl_parameter = expand.grid(
    happiness_threshold = seq(0, .8, length.out = 5)
  ),
  master_seed  = 42,
  max_cores    = 15
)

# ================= RUN SIMULATION ================= 

results <- run_simulation(simulation)

# ================= GENERATE GRAPHS ================= 

ggplot(results$df_final, aes(x = happiness_threshold, y = final_unhappy)) +
  geom_point() +
  labs(
    title = "Final Unhappiness vs Happiness Threshold",
    x = "Happiness Threshold",
    y = "Final Proportion of Unhappy Agents"
  ) +
  theme_minimal(base_size = 14)

ggplot(results$df_final, aes(x = happiness_threshold, y = final_segregation)) +
  geom_point() +
  labs(
    title = "Final Segregation vs Happiness Threshold",
    x = "Happiness Threshold",
    y = "Final Segregation Level"
  ) +
  theme_minimal(base_size = 14)


# ================= GENERATE REGRESSION TABLE ================= 
# Load packages
library(broom)
library(modelsummary)

# Create new variables for transformations
results$df_final <- results$df_final %>%
  mutate(
    log_happiness_threshold = log(happiness_threshold + 0.01),  # add small constant to avoid log(0)
    happiness_threshold_sq  = happiness_threshold^2
  )

# Model 1: Log-transformed predictor for unhappiness
model_log_happy <- lm(final_unhappy ~ log_happiness_threshold, data = results$df_final)

# Model 2: Quadratic model for segregation
model_quad_segreg <- lm(final_segregation ~ happiness_threshold + happiness_threshold_sq, data = results$df_final)

# Create a regression table with significance stars
modelsummary(
  list(
    "Unhappiness (log model)" = model_log_happy,
    "Segregation (quadratic model)" = model_quad_segreg
  ),
  statistic = "({std.error})",
  stars = TRUE,            # <- Add stars based on p-values
  gof_omit = "IC|Log|Adj",  # optional: omit information criteria if you want a cleaner table
  output = "markdown"
)
