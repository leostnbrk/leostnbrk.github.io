##################################################################
#                                                                #
# Functions for ...                                              #
# Diffusion                                                      #
#                                                                # 
# Version Diffusion-2                                            #
#                                                                #
##################################################################


get_data_replication <- function(results){
  # 1) prepare data for the generation of the data frames
  df_time_series_rep  <- map_dfr(results, `[[`, "df_time_series", .id = "rep")
  df_final_worlds_rep <- map_dfr(results, `[[`, "df_final_world", .id = "rep")
  
  replications <- length(results)
  
  # 2) since sequences can have different lengths 
  #    we need to fill in 
  max_step <- max(df_time_series_rep$step)
  df_time_series_rep_filled <- df_time_series_rep %>%
    group_by(rep) %>%
    complete(step = seq_len(max_step)) %>%     # 1,2,…,max_step
    arrange(rep, step) %>%
    fill(-rep, -step) %>%                      # fill every other column
    ungroup()
  
  # 3) statistics over the course of the simulation
  #    calculate mean and sd share at each time step
  df_time_series <- df_time_series_rep_filled |> 
    group_by(step)  |>               # then average over reps
    summarise(
      S_share_mean = mean(S, na.rm = TRUE),                            
      S_share_sd   = if (n() > 1) sd(S, na.rm = TRUE) else 0,
      I_share_mean = mean(I, na.rm = TRUE),                          
      I_share_sd   = if (n() > 1) sd(I, na.rm = TRUE) else 0,        
      R_share_mean = mean(R, na.rm = TRUE),
      R_share_sd   = if (n() > 1) sd(R, na.rm = TRUE) else 0,
      replications = replications,
      .groups = "drop"
    )
  
  # 4) return results
  list (
    df_time_series  = df_time_series,
    df_final_worlds = df_final_worlds_rep
  )
}

get_data_simulation <- function(results){
  # 1) bind all df_process together, and all df_final together
  df_names <- results %>% 
    map(~ keep(.x, is.data.frame)) %>%    # for each run, keep only data.frames
    map(names) %>%                         # extract their names
    flatten_chr() %>%                      # flatten into one character vector
    unique()                               # and dedupe
  
  # 2) for each such name, pull it out of every result and bind_rows
  out <- map(df_names, function(nm) {
    results %>% 
      map(nm) %>%                         # extract element nm from each run
      keep(is.data.frame) %>%             # drop any NULLs or non-df’s
      bind_rows()                         # stack them
  })
  
  # 3) Name the list and return
  set_names(out, df_names)
}