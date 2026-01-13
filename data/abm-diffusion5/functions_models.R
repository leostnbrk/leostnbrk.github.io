##################################################################
#                                                                #
# Functions for ...                                              #
# Diffusion                                                      #
#                                                                # 
# Version Diffusion-5                                            #
#                                                                #
##################################################################

load_model <- function(model_name) {
  
  all0 <- list(
    # --- world generation ---
    world_size    = 31,          # creates a world of 21x21 vertices
    network       = "lattice",
    network_m     = 1,           # parameter for pa network
    network_nei   = 1,           # parameter for lattice network
    transmit_prob = 0.0,
    I_S_prob      = 0.0,
    I_R_prob      = 0.0,
    I_D_prob      = 0.0,
    R_S_prob      = 0.0,
    D_S_prob      = 0.0,
    S_I_prob      = 0.0, 
    init_I        = 5,
    init_R        = 0,
    init_R_procedure = "random",
    max_t         = 250,
    # --- replication ---
    seed          = 42           # the initial seed for creating the same world over and over
  )
  
  all0_pa <- within(all0, {
    network       = "pa" 
    network_m     = 5           
  })

  SIR <- within(all0, {
    transmit_prob = 0.5
    I_R_prob      = 0.3
  })
  
  SIRS <- within(all0, {
    transmit_prob = 0.5
    I_R_prob      = 0.3
    R_S_prob      = 0.3
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
