##################################################################
#                                                                #
# Functions for ...                                              #
# extracting the data we want from the results.                  #
#                                                                # 
# Version PD-1                                                   #
#                                                                #
##################################################################


get_data_replication <- function(results){
  # 1) transform lists into matrices
  avg_fitness_mat <- do.call(rbind, lapply(results, `[[`, "avg_fitness_series"))
  top10_mat <- do.call(rbind, lapply(results, `[[`, "top10"))
  
  # 2) calculate mean and sd fitness at each time step
  df_fitness <- data.frame(
    step         = seq_len(ncol(avg_fitness_mat)),          
    mean_fitness = colMeans(avg_fitness_mat),
    sd_fitness   = apply(avg_fitness_mat, 2, sd)
  )
  
  # 3) calculate mean and sd fitness at each time step
  df_top10 <- top10_mat %>%                      
    group_by(step, strat_id) %>%                 
    summarise(
      n_agents_mean = mean(n_agents),
      n_agents_sd   = if (n() > 1) sd(n_agents) else 0,
      fitness_mean  = mean(mean_fitness),
      fitness_sd    = if (n() > 1) sd(mean_fitness) else 0,
      .groups = "drop"
    )
  
  # 4) return results
  list (
    df_fitness = df_fitness,
    df_top10 = df_top10
  )
}

get_data_simulation <- function(results){
  
  # 4) bind all df_process together, and all df_final together
  all_df_fitness <- map(results, "df_fitness") %>%
    bind_rows()
  
  all_df_top10   <- map(results, "df_top10") %>%
    bind_rows()
  
  # 5) return as a list of two data frames
  list(
    df_fitness = all_df_fitness,
    df_top10   = all_df_top10
  )
}