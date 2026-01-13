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
    # --- single game ---
    b = 3,                         # the benefit if the other cooperates
    c = 1,                         # the cost to cooperate
    # --- world generation ---
    n = 100,                       # number of agents 
    memory = 1,                    # how many rounds can actors remember? 
    expected_rounds = c(10, 20),   # an interval how many rounds of a games are to expect
    seed = 42,                     # our starting seed
    # --- step ---
    mu = 0.05,                     # 5 % of our agents mutate per time step (i.e. 5 agents)
    mu_rnd = 0.2,                  # 20% of the changers are purely random (i.e. 1 agent)
    w  = 5,                        # 
    max_t = 100                    # 100
  )
  
  # Modify the baseline model 
  # Note that in here no commas a needed between the parameters
  baseline_long <- within(baseline, {
    max_t = 750
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
