##################################################################
#                                                                #
# Functions for ...                                              #
# Calibration                                                    #
#                                                                # 
# Version Opinion-2                                              #
#                                                                #
##################################################################

as.comparable_df2 <- function(
    sim = NULL,
    emp = NULL
) {
  if (is.null(sim) & is.null(emp)) stop("This function needs either `sim` or `emp`")
  
  if (is.null(emp)) {
    opinions <- sim$df_final_world$opinions
  } else {
    opinions <- emp - 1
  }
  
  # Convert to 1 × N matrix
  opin_mat <- matrix(
    ifelse(opinions > 0.5, 1, 0),
    ncol = length(opinions)
  )
  
  # Convert to tibble with column names V1, V2, ...
  as_tibble(opin_mat, .name_repair = ~ paste0("V", seq_along(.)))
}

as.comparable_df <- function(
    sim = NULL,
    emp = NULL
) {
  if (is.null(sim) & is.null(emp)) stop("This function needs either `sim` or `emp`")
  
  if (is.null(emp)) {
    opinions_list <- sim$df_final_world$opinions
  } else {
    opinions_list <- emp
  }
  
  # 2) bind into an N×K matrix and coerce to tibble
  df <- as_tibble(do.call(rbind, opinions_list),
                  .name_repair = ~ paste0("O", seq_along(.)))
  
  # 3) compute mean & sd for each column
  summary_tbl <- df %>%
    summarise(
      across(
        everything(),
        list(mean = mean, sd = sd),
        .names = "{.col}_{.fn}"
      )
    )
  
  summary_tbl
}


get_measure_fit <- function(empirical_data, 
                            synthetic_data
                            ) {
  # 1) Compute relative squared differences
  diffs <- numeric(length(empirical_data))
  
  for (i in seq_along(empirical_data)) {
    v_emp <- as.numeric(empirical_data[i])
    v_syn <- as.numeric(synthetic_data[i])
    
    rel_sq_diff <- ((v_emp - v_syn) / ((v_emp + v_syn) / 2) )^2
    
    # Handle divide by 0 cases
    diffs[i] <- ifelse(is.nan(rel_sq_diff), 0, rel_sq_diff)
  }
  
  # Step 5: Return average fit score
  mean(diffs, na.rm = TRUE)
}
