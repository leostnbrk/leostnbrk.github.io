##################################################################
#                                                                #
# Functions for ...                                              #
# we need for plotting                                           #
#                                                                # 
# Version 1                                                      #
#                                                                #
##################################################################


plot_world <- function(world, 
                       color = c("white", "red", "blue")) {
  m <- sqrt(length(V(world)))  # infer grid size from population length
  
  # Create a data frame from population vector
  df <- data.frame(
    id   = seq_along(V(world)),
    type = factor(V(world)$type)
  )
  
  # Compute grid coordinates
  df$x <- ((df$id - 1) %% m) + 1
  df$y <- m - ((df$id - 1) %/% m)  # flip y-axis for top-down view
  
  # Plot
  ggplot(df, aes(x = x, y = y, fill = type)) +
    geom_tile(color = "black", size = 0.2) +
    coord_equal(expand = FALSE) +
    scale_fill_manual(values = color) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title   = element_blank(),
      axis.text    = element_blank(),
      axis.ticks   = element_blank(),
      panel.grid   = element_blank()
    ) +
    labs(fill = "Type")
}

