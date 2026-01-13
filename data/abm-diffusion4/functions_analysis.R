summarise_strategies_final <- function(df, top_n = 10) {      
  # 1) get the strategies
  df <- df |> 
    mutate(
      strat_id = do.call(paste0, pick(rep:fitness, -c(rep, fitness)))
    )      
  
  # 2) calculate mean fitness   
  df |>
    group_by(strat_id) |>
    summarise(
      share        = n() / nrow(df),
      fitness_mean = mean(fitness),
      .groups      = "drop"     ) |>      
    arrange(desc(fitness_mean)) |>
    slice_head(n = top_n)
} 