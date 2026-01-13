##################################################################
#                                                                #
# Functions for ...                                              #
# Diffusion                                                      #
#                                                                # 
# Version Diffusion-5                                            #
#                                                                #
##################################################################

library(tidyverse)
library(igraph)

load_type_names <- function() {
  codes <- c(1:4)
  names(codes) <- c("S", # Susceptible
                    "I", # Infectious
                    "R", # Removed (Immune)
                    "D" # Dead
                    )
  # inject into caller
  list2env(as.list(codes), envir = parent.frame())
}

get_individual_properties <- function(n) {
  tibble(
    D_vulnerability = sample(1:3, size = n, replace = TRUE)
    # Add more properties here later as needed
  )
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
  
  # 2) set individual properties
  
  # --- set days infected ---
  V(agents)$days_infected = 0
  
  # --- set D_vulnerability ---
  
  new_properties <- get_individual_properties(vcount(agents))
  # Assign each property back to the appropriate agents
  V(agents)$D_vulnerability <- new_properties$D_vulnerability

  # --- set initial S types ---
  # all agents start susceptible
  V(agents)$type <- S
  
  # --- set initial R types ---
  # how many R‐nodes to pick?
  n_R <- floor(model$init_R * vcount(agents))
  
  if (model$init_R_procedure == "random") {
    # random order
    R_indices <- sample(vcount(agents), n_R)
  } else if (model$init_R_procedure == "high_degree") {
    # order indices by decreasing degree
    R_indices <- order(degree(agents, mode = "all"), decreasing = TRUE)[seq_len(n_R)]
    
  } else if (model$init_R_procedure == "high_D_vulnerability") {
    # order by decreasing vulnerability; ties break by index
    R_indices <- order(V(agents)$D_vulnerability, decreasing = TRUE)[seq_len(n_R)]
  } else {
    stop("`init_R_procedure` must be one of 'random', 'high_degree', or 'high_D_vulnerability'")
  }
  
  V(agents)$type[R_indices] <- R

  # --- set inital I types ---
  c  <- ceiling((m+1)/2)
  ids <- seq_len(m*m)
  x   <- ((ids-1) %% m) + 1
  y   <- ((ids-1) %/% m) + 1
  V(agents)$type[ head(order(sqrt((x-c)^2 + (y-c)^2), atan2(y-c, x-c)), model$init_I) ] <- I
  
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
  old_types  <- V(agents)$type
  N          <- length(old_types)
  new_types  <- old_types   # we'll overwrite this
  
  # get all agents that are I
  infected_idx <- which(old_types == I)
  
  # +1 for the days infected 
  V(agents)$days_infected[infected_idx] <-  V(agents)$days_infected[infected_idx] + 1
  
  # 1A) Infectious nodes (I → {S,I,R,D})
  if (length(infected_idx) > 0) {
    #
    transition_prob <- matrix( 
                        c(rep(model$I_S_prob, length(infected_idx)),                    # same I_S_prob
                          rep(model$I_R_prob, length(infected_idx)),                    # same I_R_prob
                          model$I_D_prob * V(agents)$D_vulnerability[infected_idx]      # individual I_D_prob
                         ),
                        ncol = 3
                      )
    
    # check row‐sums > 1 and normalize those rows
    row_sum <- rowSums(transition_prob)
    over1   <- which(row_sum > 1)
    if (length(over1) > 0) {
      # divide each offending row by its total so that pS + pR + pD = 1
      transition_prob[over1, ] <- transition_prob[over1, ] / row_sum[over1]
    }
    
    u <- runif(length(infected_idx))
    
    cut1 <-        transition_prob[,1]
    cut2 <- cut1 + transition_prob[,2]
    cut3 <- cut2 + transition_prob[,3]
    
    new_I_states <- case_when(
      u < cut1 ~ S,  # if u[i] < cut1[i], go to S
      u < cut2 ~ R,  # else if u[i] < cut2[i], go to R
      u < cut3 ~ D,  # else if u[i] < cut3[i], go to D
      TRUE     ~ I   # otherwise, stay I
    )
    
    new_types[infected_idx] <- new_I_states
  }
  
  ## 1B) Susceptible nodes (S → {S,I})
  susceptible_idx <- which(old_types == S)
  if (length(susceptible_idx) > 0 && model$S_I_prob > 0) {
    u_S <- runif(length(susceptible_idx))
    from_S_to_I <- susceptible_idx[u_S < model$S_I_prob]
    # everyone else stays S (already new_types[...] == S)
    new_types[from_S_to_I] <- I
  }
  
  ## 1C) Removed nodes (R → {R,S})
  removed_idx <- which(old_types == R)
  if (length(removed_idx) > 0 && model$R_S_prob > 0) {
    u_R <- runif(length(removed_idx))
    from_R_to_S <- removed_idx[u_R < model$R_S_prob]
    # everyone else stays R
    new_types[from_R_to_S] <- S
  }
  
  ## 1D) Dead nodes (D → {D,S})
  dead_idx <- which(old_types == D)
  if (length(dead_idx) > 0 && model$D_S_prob > 0) {
    u_D <- runif(length(dead_idx))
    from_D_to_S <- dead_idx[u_D < model$D_S_prob]
    # everyone else stays D
    new_types[from_D_to_S] <- S
    # update `D_vulnerability`
    new_properties <- get_individual_properties(length(from_D_to_S))
    # Assign each property back to the appropriate agents
    V(agents)$D_vulnerability[from_D_to_S] <- new_properties$D_vulnerability
  }
  
  # 2) loop through all initial infectious agents
  for (v in infected_idx) {
    # infect susceptible neighbors
    for (u in nbrs[[v]]) {
      if (old_types[u] == S && runif(1) < model$transmit_prob) {
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
                            num_of_types = 4) {
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
  shares_series <- matrix(NA_real_, nrow = model$max_t, ncol = 4,
                          dimnames = list(NULL, c("S","I","R","D")))  
  # 3) iterate
  for (t in seq_len(model$max_t)) {
    # Store the data from world
    shares_series[t,] <- get_type_shares(world, 4)
    
    # stop if no infectious are left (and there is no spontaneous infection)
    if (shares_series[t, "I"] == 0 && model$S_I_prob == 0) {
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