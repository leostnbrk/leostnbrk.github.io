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

init_world <- function(model) {
  # For reproduceability we set a seed
  set.seed(model$seed)
  
  m <- model$world_size
  
  # 1) generate the lattice network 
  
  agents <- make_lattice(
    dimvector = c(m, m),
    nei        = 1
  )
  
  # 2) set the type: 1 = Susceptible, 2 = Infected, 3 = Removed
  
  # all agents start susceptible
  V(agents)$type <- 1
  
  V(agents)$type[sample(vcount(agents), floor(model$init_R * vcount(agents)))] <- 3
  
  # but for the agents in the center of the network
  c  <- ceiling((m+1)/2)
  ids <- seq_len(m*m)
  x   <- ((ids-1) %% m) + 1
  y   <- ((ids-1) %/% m) + 1
  V(agents)$type[ head(order(sqrt((x-c)^2 + (y-c)^2), atan2(y-c, x-c)), model$init_I) ] <- 2
  
  list(
    agents = agents,
    nbrs   = adjacent_vertices(agents, V(agents))
  )
}

run_step <- function(world, model){
  # get the agents and the neighbors
  agents <- world$agents
  nbrs   <- world$nbrs
  
  # get their types
  types     <- V(agents)$type
  # copy them to later update them
  new_types <- types
  
  # get all agents that are I
  infected_idx <- which(types == 2)
  
  # 1A) infectious --> removed
  if (model$I_R_prob > 0){
    # pick those where the random draw is < I_S_prob
    I_R_idx   <- infected_idx[runif(length(infected_idx))   < model$I_R_prob]
    # set them back to S all at once
    new_types[I_R_idx] <- 3
  }
  
  # 1B) removed --> susceptible
  if (model$R_S_prob > 0){
    # get all agents that are R 
    removed_idx <- which(new_types == 3)
    # pick those where the random draw is < I_S_prob
    R_S_idx   <- removed_idx[runif(length(removed_idx))   < model$R_S_prob]
    # set them back to S all at once
    new_types[R_S_idx] <- 1
  }
  
  # 1C) infectious --> susceptible
  if (model$I_S_prob > 0){
    # get all agents that are S 
    infected_without_R_idx <- which(new_types == 2)
    # pick those where the random draw is < I_S_prob
    I_S_idx   <- infected_without_R_idx[runif(length(infected_without_R_idx))   < model$I_S_prob]
    # set them back to S all at once
    new_types[I_S_idx] <- 1
  }

  # 1D) susceptible --> infectious
  if (model$S_I_prob > 0){
    # get all agents that are S 
    susceptible_idx <- which(new_types == 1)
    # pick those where the random draw is < I_S_prob
    S_I_idx   <- susceptible_idx[runif(length(susceptible_idx))   < model$S_I_prob]
    # set them back to S all at once
    new_types[S_I_idx] <- 2
  }
  
  # 2) loop through all infectious agents
  for (v in infected_idx) {
    # infect susceptible neighbors
    for (u in nbrs[[v]]) {
      if (new_types[u] == 1 && runif(1) < model$transmit_prob) {
        new_types[u] <- 2
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

run_sequence <- function(model) {
  # 1) Initialization
  world <- init_world(model)
  
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
           type      = V(world$agents)$type,                              # add agents type
           neighbors = adjacent_vertices(world$agents, V(world$agents)),  # add their neighbors
           step      = t                                                  # add how long it took to terminate
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