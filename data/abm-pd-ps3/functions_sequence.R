##################################################################
#                                                                #
# Functions for ...                                              #
# Simple Evolutionary Prisoner's Dilemma                         #
#                                                                # 
# Version PD-2                                                   #
#                                                                #
##################################################################

library(tidyverse)
library(Rcpp)
# compile the real fast c++ code
Rcpp::sourceCpp("run_step_fast.cpp")

# Function to generate a random strategy
get_random_strategy <- function(memory,
                                levels = 2 # Integer or Inf: How many values can the decision parameter take between 0 and 1? 
                                ) {
  # what are the possible outcomes of a round? 
  base_hist <- c("DD", "CD", "DC", "CC")
  
  # we start only with the very first decision
  decision_points <- "first"
  
  # generate a history vector
  if (memory > 0) {
    histories <- base_hist
    for (i in seq_len(memory)) {
      decision_points <- c(decision_points, histories)
      if (i < memory)        # build the (i + 1)-round histories for next loop
        histories <- as.vector(outer(histories, base_hist, paste0))
    }
  }
  
  ## draw one random value from `decision_pool` for each decision point
  if (is.infinite(levels) || identical(levels, "continuous")) {
    ## continuous: U(0,1)
    decisions <- runif(length(decision_points))
  } else {
    ## discrete: 0 … 1
    decision_pool <- seq(0, 1, length.out = levels)
    decisions     <- sample(decision_pool, length(decision_points), replace = TRUE)
  }

  names(decisions) <- decision_points
  decisions
}

get_transition_matrix <- function(memory) {
  base_hist <- c("DD", "CD", "DC", "CC")
  
  # we start only with the very first decision
  decision_points <- "first"
  
  histories       <- base_hist
  
  if (memory == 0) {
    return( matrix(rep(1,4),ncol=4))
  }
  
  if (memory > 0) {
    for (i in seq_len(memory)) {
      decision_points <- c(decision_points, histories)
      histories       <- as.vector(outer(histories, base_hist, paste0))
    }
  }
  
  n_state  <- length(decision_points)
  idx_of   <- setNames(seq_len(n_state), decision_points)   # label → row number

  trans_mat <- matrix(
    integer(n_state * 4L),          # rows = states, cols = outcomes 1…4
    nrow = n_state, ncol = 4L,
    dimnames = list(decision_points, NULL)
  )
  
  keep_last <- function(s) {        # keep last <memory> rounds (each is 2 chars)
    keep <- 2L * memory
    if (keep == 0L || nchar(s) <= keep) s else
      substr(s, nchar(s) - keep + 1L, nchar(s))
  }
  
  for (s in seq_len(n_state)) {
    hist_str <- decision_points[s]
    for (o in 0:3) {
      next_str <- if (hist_str == "first") {
        base_hist[o + 1L]
      } else {
        keep_last(paste0(hist_str, base_hist[o + 1L]))
      }
      trans_mat[s, o + 1L] <- idx_of[[next_str]]
    }
  }
  
  colnames(trans_mat) <- base_hist
  
  trans_mat                      # integer matrix, indices start at 1
}

init_world <- function(model) {
  set.seed(model$seed)
  n <- model$n
  
  # 1) generate random strategies 
  strat_list <- lapply(seq_len(n), function(i) get_random_strategy(model$memory))
  
  # extract strategy names
  strat_names = names(strat_list[[1]])

  # 2) transform list of strategies into a matrix (important for speed!)
  strat_mat <- matrix(
    as.integer(unlist(strat_list, use.names = FALSE)),
    nrow  = length(strat_list),
    ncol  = length(strat_names),
    byrow = TRUE,
    dimnames = list(NULL, names(strat_list[[1]]))
  )  
  # 3) assemble agents without list-column strategy
  agents <- data.frame(
    id      = seq_len(n),
    fitness = numeric(n),
    stringsAsFactors = FALSE
  )
  
  # 4) add a transition matix
  trans_mat <- get_transition_matrix(model$memory)
  
  # Return world with matrix for fast lookup
  list(
    agents      = agents,
    strat_mat   = strat_mat,
    trans_mat   = trans_mat
  )
}

run_step_fast <- function(world, model) {
  # draw random number of rounds
  rounds <-  ifelse(diff(model$expected_rounds)==0,
                    model$expected_rounds[1],
                    rounds <-sample(model$expected_rounds[1]:model$expected_rounds[2], 1L)
                   )
  
  # extract variables
  agents      <- world$agents
  strat_mat   <- world$strat_mat
  trans_mat   <- world$trans_mat
  n           <- nrow(agents)
  m           <- model$memory
  
  agents$fitness = 0
  
  # Loop over unique pairs
  for (i in 1:(n-1L)) {
    for (j in (i+1L):n) {
      
      # get their strategies
      strat_i <- strat_mat[i, ]
      strat_j <- strat_mat[j, ]
      
      # how often i/j cooperated over ALL rounds
      coop_i  <- coop_j  <- 0   
      # the history of their interaction AS STATE
      state_i <- state_j <- 1       # 1 = “first”
      
      for (r in seq_len(rounds)) {
        # determine the intended actions based on current state
        intension_i <- strat_i[state_i]
        intension_j <- strat_j[state_j]
         
        # agent executes a move, possibly with error
        action_i <- ifelse (runif(1) < model$error, sample(c(0,1),1), intension_i)
        action_j <- ifelse (runif(1) < model$error, sample(c(0,1),1), intension_j)
        
        # track cooperation (1 = cooperate, 0 = defect)
        coop_i <- coop_i + action_i
        coop_j <- coop_j + action_j
        
        # apply perceptual errors: agents may misinterpret their partner’s actual action
        perception_i_of_j <- ifelse (runif(1) < model$noise, sample(c(0,1),1), action_j)
        perception_j_of_i <- ifelse (runif(1) < model$noise, sample(c(0,1),1), action_i)
    
        # update agent state based on transition matrix
        # 1 = DD , 2 = CD , 3 = DC , 4 = CC
        # Note: transition uses the agent's *intended* action (not actual behavior), and their *perceived* view of the other
        state_i  <- trans_mat[state_i,  intension_i + 2 * perception_i_of_j + 1]
        state_j  <- trans_mat[state_j,  intension_j + 2 * perception_j_of_i + 1]
      }
      
      agents$fitness[i] <- agents$fitness[i] - model$c*coop_i + model$b*coop_j
      agents$fitness[j] <- agents$fitness[j] - model$c*coop_j + model$b*coop_i
    }
  }
  
  agents$fitness <- agents$fitness / ((n-1) * rounds)
  
  world$agents <- agents
  world
}

update_agents <- function(world, model) {
  agents    <- world$agents
  strat_mat <- world$strat_mat
  n         <- nrow(agents)
  
  # 1. Choose all agents that will change
  n_update <- round(model$mu * n)
  changers <- sample(seq_len(n), n_update)
  
  # 2. Randomly split into mutators and imitators
  if (model$mutators_n_fixed == TRUE) {
    n_mutate  <- round(model$mu_rnd * n_update)
    mutators  <- sample(changers, n_mutate)
    imitators <- setdiff(changers, mutators)
  } else {  
    mut_ids <- runif(length(changers)) < model$mu_rnd    # TRUE → mutate
    mutators  <- changers[ mut_ids ]
    imitators <- changers[!mut_ids ]
  }
  
  # 3. Imitation: sample successful agents, weighted by exp(w * fitness)
  if (length(imitators) > 0) {
    fitness_exp <- exp(model$w * agents$fitness)
    prob_weights <- fitness_exp / sum(fitness_exp)
    sources <- sample(seq_len(n), length(imitators), replace = TRUE, prob = prob_weights)
    strat_mat[imitators,] <- strat_mat[sources,]
  }
  
  # 4. Mutation: assign completely new random strategies
  if (length(mutators) > 0) {
    strat_mat[mutators,] <- matrix(unlist(lapply(seq_along(mutators), function(i) {
                                    get_random_strategy(model$memory)
                                   })),
                                  nrow = length(mutators),
                                  byrow = TRUE)
  }
  
  world$strat_mat <- strat_mat
  return(world)
}

summarise_strategies <- function(world, top_n = 10) {
  world$agents |> 
    mutate(strat_id = apply(world$strat_mat, 1L, paste0, collapse = "")) |> 
    group_by(strat_id) |>  
    summarise(
      share        = n() / nrow(world$strat_mat), # calculate the fraction of this strategy 
      fitness_mean = mean(fitness),               # calculate the mean fitness
      .groups = "drop"
    ) |> 
    arrange(desc(fitness_mean)) |> 
    slice_head(n = top_n)          # keep only the top️ rows for display
}

run_sequence <- function(model, world = NULL) {
  # 1) initialization (if not provided)
  if (is.null(world)) world <- init_world(model)
  
  # 2) prepare data storage
  population_fitness_series     <- integer(0)  # population fitness over time steps
  strategies_performance_series <- list()      # performance of strategies over time steps
  
  # 3) iterate
  for (t in seq_len(model$max_t)) {
    # Perform one step (and update metrics inside world)
    world <- run_step_pC_err_cpp(world, model)
    
    # Store the data from world
    population_fitness_series[t]       <- mean(world$agents$fitness)
    strategies_performance_series[[t]] <- summarise_strategies(world, 32)
    
    # do not update agents in the last round
    if (t < model$max_t) world <- update_agents(world, model)
  }
  
  df_population <- data.frame(
    step         = seq_len(model$max_t),          
    pop_fit_mean = population_fitness_series
  )
  
  df_strategies <- bind_rows(strategies_performance_series, .id = "step") |>
    mutate(step = as.integer(step))   
  
  df_final_world <- bind_cols(
    world$strat_mat,   
    fitness = world$agents$fitness  
  )
  
  # 4) return final state and summary
  list(
    df_final_world = df_final_world,
    df_population  = df_population,
    df_strategies  = df_strategies
  )
}
