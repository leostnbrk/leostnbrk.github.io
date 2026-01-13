##################################################################
#                                                                #
# Functions for ...                                              #
# Opinion dynamics                                               #
#                                                                # 
# Version Opinion-2                                              #
#                                                                #
##################################################################

load_model <- function(model_name) {
  
  baseline <- list(
    # --- world generation ---
    N                 = 100,           # number of agents 
    network           = "pa",          # "pa"
    network_m         = 1,             # only for PA
    opinions_num      = 2,             # number of opinion dimensions per agent
    opinions_type     = "interval",    # "category" or "interval"
    opinions_min      = 0,             # 
    opinions_max      = 1,             #
    influence         = "bounded_confidence", # "bounded_confidence" or "linear_decline"
    influence_metric  = "taxicab",     # "dimension_wise", "taxicab" or "euclidean" 
    influence_mu      = 0.4,           # the μ parameter
    influence_epsilon = 1,             # only used for bounded_confidence
    max_t             = 100,           # # of maximal time steps
    seed              = 123            # for reproducibility
  )
  
  karate <- list(
    # --- world generation ---
    N                 = 34,            # number of agents 
    network           = "karate",      # predefined karate network
    opinions_num      = 1,             # number of opinion dimensions per agent
    opinions_type     = "interval",    # "category" or "interval"
    opinions_min      = 0,             # 
    opinions_max      = 1,             #
    influence         = "bounded_confidence", # "bounded_confidence" or "linear_decline"
    influence_metric  = "euclidean",   # "dimension_wise", "taxicab" or "euclidean" 
    influence_mu      = .35,            # the μ parameter
    influence_epsilon = .4,             # only used for bounded_confidence
    
    max_t             = 100,           # # of maximal time steps
    seed              = 123            # for reproducibility
  )
  
  special <- list(
    # --- world generation ---
    N                 = 100,           # number of agents 
    network           = "pa",          # "pa"
    network_r         = 1,
    network_m         = 1,             # only for PA
    opinions_num      = 5,             # number of opinion dimensions per agent
    opinions_type     = "interval",    # "category" or "interval"
    opinions_min      = 0,             # 
    opinions_max      = 1,             #
    influence         = "bounded_confidence", # "bounded_confidence" or "linear_decline"
    influence_metric  = "euclidean",     # "dimension_wise", "taxicab" or "euclidean" 
    influence_mu      = .7,           # the μ parameter
    influence_epsilon = 1,             # only used for bounded_confidence
    max_t             = 600,           # # of maximal time steps
    seed              = 123            # for reproducibility
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
