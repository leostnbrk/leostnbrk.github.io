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

