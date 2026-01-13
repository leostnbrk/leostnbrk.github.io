##################################################################
#                                                                #
# Functions for ...                                              #
# extracting the data we want from the results.                  #
#                                                                # 
# Version 1                                                      #
#                                                                #
##################################################################


get_data_replication <- function(results){
  # extract series and align by length
  max_steps <- max(sapply(results, function(x) length(x$unhappy_series)))
  
  # helper to fill with the last value if run ended early
  fill_with_last <- function(series, len) {
    c(series, rep(tail(series, 1), len - length(series)))
  }
  
  # generate data matrices 
  unhappy_mat <- matrix(sapply(results, function(x) fill_with_last(x$unhappy_series, max_steps)), nrow = max_steps)
  segreg_mat  <- matrix(sapply(results, function(x) fill_with_last(x$segregation_series, max_steps)), nrow = max_steps)
  
  # 3) calculate mean and sd at each time step
  df_sequence <- data.frame(
    step             = seq_len(max_steps),
    unhappy_mean     = apply(unhappy_mat, 1, mean, na.rm = TRUE),
    unhappy_sd       = apply(unhappy_mat, 1, sd  , na.rm = TRUE),
    segregation_mean = apply(segreg_mat , 1, mean, na.rm = TRUE),
    segregation_sd   = apply(segreg_mat , 1, sd  , na.rm = TRUE)
  )
  
  # 4) calculate the final values for each replication
  df_final <- data.frame(
    replication       = seq_len(length(results)),
    converged         = sapply(results, `[[`, "converged"),
    steps_taken       = sapply(results, `[[`, "steps_taken"),
    final_segregation = sapply(results, function(x) tail(x$segregation_series, 1)),
    final_unhappy     = sapply(results, function(x) tail(x$unhappy_series,     1))
  )
  
  # 5) return results
  list(
    df_sequence = df_sequence,
    df_final    = df_final
  )
}

get_data_simulation <- function(results){
  
  # 4) bind all df_process together, and all df_final together
  all_df_sequence <- map(results, "df_sequence") %>%
    bind_rows()
  
  all_df_final   <- map(results, "df_final") %>%
    bind_rows()
  
  # 5) return as a list of two data frames
  list(
    df_sequence = all_df_sequence,
    df_final   = all_df_final
  )
}