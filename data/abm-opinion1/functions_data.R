##################################################################
#                                                                #
# Functions for ...                                              #
# Diffusion                                                      #
#                                                                # 
# Version Diffusion-5                                            #
#                                                                #
##################################################################


get_data_replication <- function(results){
  # 1) prepare data for the generation of the data frames
  df_time_series_rep  <- map_dfr(results, `[[`, "df_time_series", .id = "rep")
  df_final_worlds_rep <- map_dfr(results, `[[`, "df_final_world", .id = "rep")
  
  replications <- length(results)
  
  # 2) statistics over the course of the simulation
  #    calculate mean and sd share at each time step

  df_time_series <- df_time_series_rep |>
    group_by(step) |>
    summarise(
      replications = n(),
      across(
        .cols = c(-replications, -rep),   # exclude the .id column
        .fns  = list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ if (n() > 1) sd(.x, na.rm = TRUE) else 0
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  # 5) return results
  list (
    df_time_series   = df_time_series,
    df_final_worlds  = df_final_worlds_rep
  )
}

get_data_simulation <- function(results){
  # 1) bind all df_process together, and all df_final together
  df_names <- results |> 
    map(~ keep(.x, is.data.frame)) |>  # for each run, keep only data.frames
    map(names) |>                      # extract their names
    unlist(use.names = FALSE) |>       # flatten into one character vector
    unique()                           # and dedupe
  
  # 2) for each such name, pull it out of every result and bind_rows
  out <- map(df_names, function(nm) {
    results |> 
      map(nm) |>                         # extract element nm from each run
      keep(is.data.frame) |>             # drop any NULLs or non-df’s
      bind_rows()                         # stack them
  })
  
  # 3) Name the list and return
  set_names(out, df_names)
}