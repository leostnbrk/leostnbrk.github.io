##################################################################
#                                                                #
# Functions for ...                                              #
# storing and loading our models                                 #
#                                                                # 
# Version 1                                                      #
#                                                                #
##################################################################


load_model <- function(model_name) {
  
  # define a baseline model 
  baseline <- list(
    # --- world generation ---
    world_size   = 10,          # creates a world of 10x10 vertices
    world_empty  = 0.2,         # 20% of the grid is empty
    pop_shares   = c(0.5, 0.5), # equal population sizes for two groups
    seed         = 42,          # 
    # --- run ---
    happiness_threshold = 0.5,  # A agent is happy if at least 50% of vertices have the same color 
    max_t   = 250               # Stop the run after 250 steps
  )
  
  # Modify the baseline model 
  # Note that in here no commas a needed between the parameters
  baseline_big <- within(baseline, {
    world_size       = 100        
  })
  
  if (exists(model_name, inherits = FALSE)) {
    # fetch the model
    model <- get(model_name)
    # add a name
    model$name = model_name
    # return
    model
  } else {
    stop("Error: Model name not found.")
  }  
}