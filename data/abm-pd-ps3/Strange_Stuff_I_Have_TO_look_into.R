rm(list = ls())

library(tidyverse)

source("functions_sequence.R")
source("functions_plot.R")
source("functions_analysis.R")

model <- list(
  # --- single game ---
  b = 3,                         
  c = 1,                         
  # --- world generation ---
  n = 25,                        # only 25 agents !
  memory = 1,                    
  expected_rounds = c(50, 50),   
  seed = 1234,                   
  error = 0,                     
  noise = 0,                     
  # --- step ---
  mu = 0.05,                     
  mu_rnd = 0.00,                 # no random mutation !
  w  = 5,                       
  mutators_n_fixed = FALSE,      
  max_t = 100                   
)

world <- init_world(model)

best_strat <- matrix(rep(NA, (model$n + 1) * (model$n + 1)), nrow = model$n + 1)

for (n_aC in 0:25) {
  for (n_TFT in 0:(25-n_aC)) {
    # prepare world
    world$strat_mat[,] <- c(0,0,0,0,0)
    if (n_aC  > 0) world$strat_mat[1:n_aC,] <- c(1,1,1,1,1)
    if (n_TFT > 0) world$strat_mat[(1+n_aC):(n_aC+n_TFT),] <- c(1,0,0,1,1)
    
    results <- run_sequence(model, world)                                   # updated the run_sequence() function, so a given world can be used instead of a random!
    best_strat[n_aC + 1, n_TFT + 1] <- results$df_final_world |> 
      mutate( 
        strat_id = do.call(paste0, pick(first:CC))
      ) |>
      group_by(strat_id) |>
      summarise(
        share        = n() / nrow(results$df_final_world),
        fitness_mean = mean(fitness),
        .groups      = "drop"     ) |>      
      arrange(desc(share), desc(fitness_mean)) |>
      slice_head(n = 1) |> 
      pull(strat_id)
   }
}
################################
################################
################################
################################  WE SHOULD ONLY GET aC, aD and TFT BUT WE GET OTHER STRATS TOOO ???!?!?!?!?
################################
################################

#install.packages("ggtern")  # uncomment if you don’t have it
library(ggtern)

# Suppose best_strat is your (n+1)×(n+1) character matrix,
# with rows = n_aC+1, cols = n_TFT+1, values in {"00000","11111","10011"}.

# 1. Build a data.frame of all (n_aC, n_TFT) grid points
n <- nrow(best_strat) - 1
grid <- expand.grid(n_aC = 0:n, n_TFT = 0:n) %>%
  filter(n_aC + n_TFT <= n) %>%
  mutate(
    n_aD       = n - n_aC - n_TFT,
    best_strat = best_strat[cbind(n_aC + 1, n_TFT + 1)],
    # turn counts into proportions
    p_aC = n_aC / n,
    p_TFT = n_TFT / n,
    p_aD = n_aD / n
  )

# 2. Map the codes to nice factor labels
grid <- grid %>%
  mutate(
    Strategy = factor(best_strat,
                      levels = c("00000","11111","10011"),
                      labels = c("aD","aC","TFT"))
  )

# 3. Plot with ggtern
ggtern(data = grid,
       aes(x = p_aC,   # left axis
           y = p_TFT,  # right axis
           z = p_aD,   # top axis
           fill = Strategy)) +
  geom_point(shape = 21, size = 4, colour = "black") +
  scale_fill_manual(
    values = c(aD   = "#1f77b4",  # blue
               aC   = "#2ca02c",  # green
               TFT  = "#d62728")  # red
  ) +
  labs(
    title = "Dominant Strategy Across Initial Population Mix",
    fill  = "Best Strategy",
    T     = "aD\n(proportion)",    # top axis label
    L     = "aC\n(proportion)",    # left axis label
    R     = "TFT\n(proportion)"    # right axis label
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
