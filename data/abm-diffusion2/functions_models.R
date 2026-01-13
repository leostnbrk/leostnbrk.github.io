##################################################################
#                                                                #
# Functions for ...                                              #
# Diffusion                                                      #
#                                                                # 
# Version Diffusion-2                                            #
#                                                                #
##################################################################

load_model <- function(model_name) {
  
  # define a baseline model 
  baseline_SIR <- list(
    # --- world generation ---
    world_size    = 31,          # creates a world of 21x21 vertices
    transmit_prob = 0.5,
    I_R_prob      = 1.0,
    I_S_prob      = 0.0,
    R_S_prob      = 0.0,
    S_I_prob      = 0.0,    
    init_I        = 1,
    init_R        = 0,
    max_t         = 200,
    # --- replication ---
    seed          = 42           # the initial seed for creating the same world over and over
  )
  
  baseline_SIS <- list(
    # --- world generation ---
    world_size    = 31,          # creates a world of 21x21 vertices
    transmit_prob = 0.5,
    I_R_prob      = 0.0,
    I_S_prob      = 1.0,
    R_S_prob      = 0.0,
    S_I_prob      = 0.0, 
    init_I        = 1,
    init_R        = 0,
    max_t         = 200,
    # --- replication ---
    seed          = 42           # the initial seed for creating the same world over and over
  )
  
  week07_inclass <- list(
    # --- world generation ---
    world_size    = 31,          # creates a world of 21x21 vertices
    transmit_prob = 1,
    I_R_prob      = 0.0,
    I_S_prob      = 0.0,
    R_S_prob      = 0.0,
    S_I_prob      = 0.0, 
    init_I        = 1,
    init_R        = 0,
    max_t         = 100,
    # --- replication ---
    seed          = 42           # the initial seed for creating the same world over and over
  )
  
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
