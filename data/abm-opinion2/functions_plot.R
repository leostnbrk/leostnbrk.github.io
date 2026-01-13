##################################################################
#                                                                #
# Functions for ...                                              #
# Diffusion                                                      #
#                                                                # 
# Version Diffusion-5                                            #
#                                                                #
##################################################################

library(rlang)

plot_world <- function(world, 
                       opinion = 1, # which opinion to plot
                       opinion_min = 0,
                       opinion_max = 1,
                       colors  = c("red", "blue"),
                       layout  = NULL
                       ){
  g  <- world$agents
  o  <- (map_dbl(V(g)$opinions, opinion) - opinion_min) / (opinion_max - opinion_min)
  
  # 2) build a ramp function and get hex colors
  pal_rgb    <- grDevices::colorRamp(colors)   # returns 0–255 RGB
  cols_rgb   <- pal_rgb(o)
  vertex_cols <- grDevices::rgb(
    cols_rgb[,1], cols_rgb[,2], cols_rgb[,3],
    maxColorValue = 255
  )
  
  if (is.null(layout)) layout = layout_with_fr(g)
  
  plot(g,
       vertex.color = vertex_cols,
       vertex.frame.color = NA,           
       vertex.size  = 186 / sqrt(length(world$agents)) / 2,
       vertex.label = NA,
       edge.color   = "grey80",
       layout       = layout
  )
}

plot_calibration_heatmap <- function(
  df,
  x_var       = "network_r",                 # Name of the column to use for x-axis
  y_var       = "influence_epsilon",         # Name of the column to use for y-axis
  fill_var    = "fit",                       # Name of the column to use for the fill (e.g. fit/error)
  palette     = "RdYlBu",                    # Brewer palette name (see ?RColorBrewer)
  direction   = -1,                          # 1 or -1, flips the palette direction
  plot_title  = "Calibration Heatmap",
  fill_transform = c("none", "log", "log10", "revlog")
) {
  fill_transform <- match.arg(fill_transform)
  
  # Apply the chosen transform
  if (fill_transform == "log") {
    df[[fill_var]] <- log(df[[fill_var]])
  } else if (fill_transform == "log10") {
    df[[fill_var]] <- log10(df[[fill_var]])
  } else if (fill_transform == "revlog") {
    df[[fill_var]] <- -log(df[[fill_var]])
  }
  
  ggplot(df, aes_string(x = x_var, y = y_var, fill = fill_var)) +
    geom_tile() +
    scale_fill_distiller(
      palette   = palette,
      direction = direction,
      name      = if (fill_transform == "none") fill_var
      else paste0(fill_var, "\n(", fill_transform, ")")
    ) +
    labs(
      title = plot_title,
      x     = x_var,
      y     = y_var
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    # treat axes as discrete so each grid line is exactly your parameter values
    scale_x_continuous(breaks = sort(unique(df[[x_var]]))) +
    scale_y_continuous(breaks = sort(unique(df[[y_var]])))
}

