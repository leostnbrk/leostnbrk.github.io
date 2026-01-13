##################################################################
#                                                                #
# Functions for ...                                              #
# Diffusion                                                      #
#                                                                # 
# Version Diffusion-3                                            #
#                                                                #
##################################################################

library(tidyverse)
library(igraph)

load_type_names <- function() {
  codes <- c(1:5,9)
  names(codes) <- c("S", # Susceptible
                    "I", # Infectious
                    "R", # Removed (Immune)
                    "E", # Exposed, infected, but yet not Infectious
                    "D", # Dead
                    "X"  # Obstacle (behaves like dead but cannot be replaced)
                    )
  # inject into caller
  list2env(as.list(codes), envir = parent.frame())
}

init_world <- function(model) {
  # For reproduceability we set a seed
  set.seed(model$seed)
  
  load_type_names()
  
  m <- model$world_size
  
  # 1) generate network 
  if (model$network == "lattice") {
    agents <- make_lattice(
      dimvector = c(m, m),
      nei        = if (exists("model") && "nei" %in% names(model)) model$network_nei else 1
    )
  } else if (model$network == "pa") {
    agents <- sample_pa(n = m*m,
                        m = if (exists("model") && "network_m" %in% names(model)) model$network_m else 1,
                        directed = FALSE)
  } else {
    cat(pasrte0("network typ ", model$network, " not supported! Used ‘lattice’ instead."))
    model$network = "lattice"
    agents <- make_lattice(
      dimvector = c(m, m),
      nei        = 1
    )
  }
  
  # 2) set the type: 1 = Susceptible, 2 = Infected, 3 = Removed
  
  # all agents start susceptible
  V(agents)$type <- S
  
  V(agents)$type[sample(vcount(agents), floor(model$init_R * vcount(agents)))] <- R
  
  # but for the agents in the center of the network
  c  <- ceiling((m+1)/2)
  ids <- seq_len(m*m)
  x   <- ((ids-1) %% m) + 1
  y   <- ((ids-1) %/% m) + 1
  V(agents)$type[ head(order(sqrt((x-c)^2 + (y-c)^2), atan2(y-c, x-c)), model$init_I) ] <- I
  V(agents)$days_infected = 0
  
  list(
    agents = agents,
    nbrs   = adjacent_vertices(agents, V(agents))
  )
}

run_step <- function(world, model){
  # get the agents and the neighbors
  agents <- world$agents
  nbrs   <- world$nbrs
  
  load_type_names()
  
  # get their types
  types     <- V(agents)$type
  # copy them to later update them
  new_types <- types
  
  # get all agents that are I
  infected_idx <- which(types == I)
  
  # count the days infected 
  V(agents)$days_infected[infected_idx] <-  V(agents)$days_infected[infected_idx] + 1
  
  # 1A) infectious --> removed
  if (model$I_R_prob > 0){
    # pick those where the random draw is < I_S_prob
    I_R_idx   <- infected_idx[runif(length(infected_idx))   < model$I_R_prob]
    # set them back to S all at once
    new_types[I_R_idx] <- R
  }

  # 1B) removed --> susceptible
  if (model$R_S_prob > 0){
    # get all agents that are R 
    removed_idx <- which(new_types == R)
    # pick those where the random draw is < I_S_prob
    R_S_idx   <- removed_idx[runif(length(removed_idx))   < model$R_S_prob]
    # set them back to S all at once
    new_types[R_S_idx] <- S
  }
  
  # 1C) infectious --> susceptible
  if (model$I_S_prob > 0){
    # get all agents that are S 
    infected_without_R_idx <- which(new_types == I)
    # pick those where the random draw is < I_S_prob
    I_S_idx   <- infected_without_R_idx[runif(length(infected_without_R_idx))   < model$I_S_prob]
    # set them back to S all at once
    new_types[I_S_idx] <- S
  }

  # 1D) susceptible --> infectious
  if (model$S_I_prob > 0){
    # get all agents that are S 
    susceptible_idx <- which(new_types == S)
    # pick those where the random draw is < I_S_prob
    S_I_idx   <- susceptible_idx[runif(length(susceptible_idx))   < model$S_I_prob]
    # set them back to S all at once
    new_types[S_I_idx] <- I
  }
  
  # 2) loop through all initial infectious agents
  for (v in infected_idx) {
    # infect susceptible neighbors
    for (u in nbrs[[v]]) {
      if (new_types[u] == S && runif(1) < model$transmit_prob) {
        new_types[u] <- I
      }
    }
  }
  
  # update agents and world
  V(agents)$type <- new_types
  world$agents   <- agents
  
  return(world)
}

get_type_shares <- function(world,
                            num_of_types = 3) {
  # get the types as a vector
  types   <- V(world$agents)$type
  
  # count how many of each type 
  counts <- tabulate(types, nbins = num_of_types)
  
  # return proportions S, I, R
  counts / length(types)
}

run_sequence <- function(model,
                         world = NULL
                         ) {
  # 1) Initialization
  if (is.null(world)) world <- init_world(model)
  
  # 2) prepare data storage
  shares_series <- matrix(NA_real_, nrow = model$max_t, ncol = 3,
                          dimnames = list(NULL, c("S","I","R")))  
  # 3) iterate
  for (t in seq_len(model$max_t)) {
    # Store the data from world
    shares_series[t,] <- get_type_shares(world, 3)
    
    # stop if no infectiouse are left
    if (shares_series[t, "I"] == 0) {
      break
    }
    
    # Perform one step
    world <- run_step(world, model)
  }
  
  # prepare the final world data
  df_final_world <- as_data_frame(world$agents, what = "vertices") |> 
    mutate(id = row_number(),                                             # add id
           type          = V(world$agents)$type,                              # add agents type
           days_infected = V(world$agents)$days_infected,                     # add infection days
           neighbors     = adjacent_vertices(world$agents, V(world$agents)),  # add their neighbors
           step          = t                                                  # add how long it took to terminate
    )
  
  # prepare the time series data
  df_time_series <- shares_series |>
    as.tibble() |>                     # turn from matrix into data frame
    filter(!is.na(S)) |>               # drop all empty rows
    mutate(step = row_number())        # add time
  
  # 3) Return final state and summary
  list(
    df_final_world  = df_final_world,
    df_time_series  = df_time_series
  )
}