##################################################################
#                                                                #
# Functions for ...                                              #
# extracting the data we want from the results.                  #
#                                                                # 
# Version PD-2                                                   #
#                                                                #
##################################################################


get_data_replication <- function(results){
  # 1) prepare data for the generation of the data frames
  #    i.e., transform lists into matrices / df 
  df_population_rep <- map_dfr(results, `[[`, "df_population", .id = "rep")
  df_strategies_rep <- map_dfr(results, `[[`, "df_strategies", .id = "rep")
  df_final_worlds_rep <- map_dfr(results, `[[`, "df_final_world", .id = "rep")
  
  # right now strategies with share 0 are missing in the df
  df_strategies_rep_filled <- df_strategies_rep %>%
    complete(rep, step, strat_id,
             fill = list(share = 0, fitness_mean = NA))
  
  # 2) statistics at the population level
  #    specifically, mean and sd fitness of the hole population at each time step
  df_population <- df_population_rep |> 
    group_by(step) |> 
    summarise(
      pop_fit_sd   = sd(pop_fit_mean),
      pop_fit_mean = mean(pop_fit_mean),
      .groups = "drop"
    )
  
  # 3) statistics at the individual level (strategies)
  #    calculate mean and sd fitness at each time step
  df_strategies <- df_strategies_rep_filled |> 
    group_by(step, strat_id)  |>               # then average over reps
    summarise(
      strat_share_mean      = mean(share, na.rm = TRUE),                                    # mean fraction of this strategy 
      strat_share_sd        = if (n() > 1) sd(share, na.rm = TRUE) else 0,
      strat_fit_mean        = mean(fitness_mean, na.rm = TRUE),                             # unweighted mean fitness
      strat_fit_sd          = if (n() > 1) sd(fitness_mean, na.rm = TRUE) else 0,          
      strat_weight_fit_mean = mean(share * fitness_mean, na.rm = TRUE),                     # weighted (by share) mean fitness
      strat_weight_fit_sd   = if (n() > 1) sd(share * fitness_mean, na.rm = TRUE) else 0,          
      .groups = "drop"
    )
  
  # 4) return results
  list (
    df_population = df_population,
    df_strategies = df_strategies,
    df_final_worlds = df_final_worlds_rep
  )
}

get_data_simulation <- function(results){
  # 1) bind all df_process together, and all df_final together
  all_df_population <- map(results, "df_population") %>%
    bind_rows()
  
  all_df_strategies <- map(results, "df_strategies") %>%
    bind_rows()

  all_df_final_worlds <- map(results, "df_final_worlds") %>%
    bind_rows()
  
    
  # 2) return as a list of two data frames
  list(
    df_population   = all_df_population,
    df_strategies   = all_df_strategies,
    df_final_worlds = all_df_final_worlds
  )
}