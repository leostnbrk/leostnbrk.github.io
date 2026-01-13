##################################################################
#                                                                #
# Functions for ...                                              #
# we need for plotting                                           #
#                                                                # 
# Version PD-2                                                   #
#                                                                #
##################################################################

if (!requireNamespace("rlang", quietly = TRUE)) {
  install.packages("rlang")
}

library(rlang)

  

plot_avg_fitness <- function(df, 
                             group_var, 
                             replications = 1 # on how many replications is 
                             ) {
  # try to fetch the number of replications in the data frame
  if (!is.null(results$df_population$replications)) {
    replications = max(results$df_population$replications)
  } 
  
  # get the group_var
  group_var <- enquo(group_var)
  
  df %>% 
    mutate(
      ci_lower = pop_fit_mean - 1.96 * (pop_fit_sd / sqrt(replications)),
      ci_upper = pop_fit_mean + 1.96 * (pop_fit_sd / sqrt(replications))
    ) %>% 
    ggplot(aes(x = step,
               y = pop_fit_mean,
               colour = factor(!!group_var),
               fill   = factor(!!group_var))) +
    geom_line() +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                alpha = 0.20, colour = NA) +
    labs(
      title = "Average fitness over time",
      x = "Time step",
      y = "Average fitness",
      colour = as_name(group_var),
      fill   = as_name(group_var)
    ) +
    theme_minimal()
}
