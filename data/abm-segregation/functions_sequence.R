##################################################################
#                                                                #
# Functions for ...                                              #
# running a complete sequence based on a model                   #
#                                                                # 
# Version 3                                                      #
#                                                                #
##################################################################

library(igraph)
library(tidyverse)

init_world <- function(model) {
  # For reproduceability we set a seed
  set.seed(model$seed)

  m <- model$world_size
  n <- m * m

  # 1) Generate the world
  
  world <- make_lattice(
    dimvector = c(m, m),
    nei        = 1
  )
  
  # Calc how many vertices will be populated by agents or stay empty
  pop_counts  <- round((1 - model$world_empty) * n * model$pop_shares)
  empty_count <- n - sum(pop_counts)
  
  # Build the vector of vertice‐types: 0 = empty, 1..k = group labels
  vertice_pool <- c(
    rep(0, empty_count),
    unlist(lapply(seq_along(pop_counts),
                  function(i) rep(i, pop_counts[i])))
  )

  # Randomly shuffle `vertice_pool` and add to world as type
  V(world)$type <- sample(vertice_pool, size = n)
  
  # init some variables for the model
  unhappy <- get_unhappy_agents(world, model)
  world$data_unhappy     <- length(unhappy) / length(which(V(world)$type != 0))
  world$data_segregation <- get_data_segregation(world)
  
  # since the network is fixed we can store this result for later use (speeds up our code)
  world$nbrs <- adjacent_vertices(world, V(world))
  
  world
}

get_unhappy_agents <- function(world, model) {
  # 1) Pull out the per-vertex types (0 = empty, 1..k = agent types)
  types <- V(world)$type

  # 2) Get the list of neighbors for each vertex
  nbrs <- world$nbrs

  # 3) Threshold
  h_t <- model$happiness_threshold

  # 4) For each agent, compute if unhappy
  is_unhappy <- vapply(
    seq_along(types),
    FUN = function(i) {
      t_i <- types[i]
      if (t_i == 0) return(FALSE)    # skip empty

      neigh_vs <- nbrs[[i]]
      # get only occupied neighbors
      occ_neigh <- neigh_vs[types[neigh_vs] > 0]
      if (length(occ_neigh) == 0) return(FALSE)  # no neighbors → happy

      same_type <- sum(types[occ_neigh] == t_i)
      frac_same <- same_type / length(occ_neigh)
      frac_same < h_t
    },
    FALSE
  )

  # 5) return the indices of unhappy agents
  which(is_unhappy)
}

run_step <- function(world, model) {
  # pull out types & unhappy list
  unhappy <- get_unhappy_agents(world, model)
  
  # calcualte data BEFORE the movement
  world$data_unhappy     <- length(unhappy) / length(which(V(world)$type != 0))
  world$data_segregation <- get_data_segregation(world)
  
  # are all actors happy?
  if (length(unhappy) == 0) {
    return(world = world)
  }
  
  # ge types
  types   <- V(world)$type
  # randomize move order
  unhappy <- sample(unhappy)
  # track current empty vertices
  empties <- which(types == 0)
  
  # choose for each agent a random location
  for (agent in unhappy) {
    # if no empties left, bail out
    if (length(empties) == 0) break
    
    # choose a random empty vertice
    target <- sample(empties, 1)
    
    # move the agent
    types[target] <- types[agent]
    types[agent]  <- 0L
    
    # update empties in‐place: replace the used slot with the old agent index
    idx <- match(target, empties)
    empties[idx] <- agent
  }
  
  # update all agents' types
  V(world)$type <- types
  
  world
}

get_data_segregation <- function(world) {
  types <- V(world)$type
  nbrs <- world$nbrs

  # for each occupied vertice, compute fraction of same‐type neighbors
  fracs <- vapply(seq_along(types), function(i) {
    t_i <- types[i]
    if (t_i == 0L) return(NA_real_)    # skip empties
    
    neigh <- nbrs[[i]]
    occ   <- neigh[types[neigh] > 0L]
    if (length(occ) == 0) return(1)    # isolated → perfect “self‐sorting”
    
    sum(types[occ] == t_i) / length(occ)
  }, numeric(1))
  
  mean(fracs, na.rm = TRUE)
}

run_sequence <- function(model,
                         verbose   = FALSE  # print progress?
) {
  # 1) Initialization
  world <- init_world(model)
  
  # 2) prepare data storage
  unhappy_series   <- integer(0)
  segregation_series <- numeric(0)
  
  
  # 3) Iterate
  for (t in seq_len(model$max_t)) {
    # Perform one step (and update metrics inside world)
    world <- run_step(world, model)
    
    # Store the data from world
    unhappy_series[t]     <- world$data_unhappy
    segregation_series[t] <- world$data_segregation 
    
    # print the results step by step if wanted
    if (verbose) {
      message(sprintf(
        "Step %3d: unhappiness = %3.0f, segregation = %.3f",  
        t, unhappy_series[t], segregation_series[t]
      ))
    }
    
    # stop if equilibrium reached
    if (unhappy_series[t] == 0) {
      if (verbose) message("All agents are happy. Stopping.")
      break
    }
  }
  
  # 3) Return final state and summary
  list(
    final_world        = world,
    unhappy_series     = unhappy_series,
    segregation_series = segregation_series,
    steps_taken        = t,
    converged          = unhappy_series[t] == 0
  )
}

