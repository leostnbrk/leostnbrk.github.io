library(tidyverse)
source("functions_models.R")
source("functions_simulation.R")

results <- run_simulation(simulation)

df <- results$df_fitness %>% 
  mutate(
    # 1. collapse the pair into one string, e.g. "2-2"
    expected_id = sapply(expected_rounds, paste, collapse = "-"),
    
    # 2. convert to factor (levels are assigned in first-seen order)
    expected_fac = factor(expected_id)
  )

p <- ggplot(df %>%
         mutate(
           ci_lower = mean_fitness - 1.96 * (sd_fitness / sqrt(simulation$replications)),
           ci_upper = mean_fitness + 1.96 * (sd_fitness / sqrt(simulation$replications))
         ), aes(x = step, y = mean_fitness, color = factor(expected_fac), fill = factor(expected_fac))) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  labs(
    title = "Average Fitness Over Time Steps",
    x = "Time Step",
    y = "Average Fitness",
    color = "expected_rounds[1]",
    fill = "expected_rounds[1]"
  ) +
  theme_minimal()