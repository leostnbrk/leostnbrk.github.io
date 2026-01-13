##################################################################
#                                                                #
# Functions for ...                                              #
# Simple Evolutionary Prisoner's Dilemma                         #
#                                                                # 
# Version 1                                                      #
#                                                                #
##################################################################

library(tidyverse)
library(Rcpp)
# compile the real fast c++ code
Rcpp::sourceCpp("run_step_fast.cpp")

# Function to generate a random strategy
get_random_strategy <- function(memory) {
  base_hist <- c("DD", "CD", "DC", "CC")
  
  # we start only with the very first decision
  decision_points <- "first"
  
  # memory == 0: only ever 'first'
  if (memory == 0) {
    return(setNames(sample(0:1, 1, replace = TRUE), decision_points))
  }
  
  # start with just 'first' + the 1-round histories
  histories       <- base_hist
  
  for (i in seq_len(memory)) {
    if (i == 1) {
      # first pass: just the 1-round histories
      decision_points <- c(decision_points, histories)
    } else {
      # build the i-round histories by pasting each (i-1)-round onto every 1-round
      histories <- as.vector(outer(histories, base_hist, paste0))
      decision_points <- c(decision_points, histories)
    }
  }
  
  # draw a random value for each decision point
  decisions <- sample(0:1, length(decision_points), replace = TRUE)
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

run_step <- function(world, model) {
  # draw random number of rounds
  rounds <- if (diff(model$expected_rounds)==0) model$expected_rounds[1]
   else sample(model$expected_rounds[1]:model$expected_rounds[2], 1L)
  
  # extract variables
  agents    <- world$agents
  strat_mat <- world$strat_mat
  n         <- nrow(agents)
  m         <- model$memory
  
  push_history <- function(x,        # new outcome
                           history,  # past history
                           m         # memory limit
                          ) {
    c(tail(history, m-1), x)
  }
  
  # Loop over unique pairs
  for (i in 1:(n-1L)) {
    for (j in (i + 1L):n) {
      
      # get their strategies
      strat_i <- strat_mat[i, ]
      strat_j <- strat_mat[j, ]
      
      # how often i/j cooperated over ALL rounds
      coop_i  <- coop_j  <- 0   
      # the history of their interaction
      history_i <- history_j <- c()          
      
      # Play rounds
      for (r in seq_len(rounds)) {
        # determine which column of the strategy to select for i
        if (is.null(history_i)) {
          col_i <- 1L
        } else {
          h_i <- length(history_i-1)
          # we derive from the past history at the correct column of the strategy
          col_i <- 1 + (4^h_i - 1)/3 + sum((history_i-1L) * 4^(0:(h_i-1)))
        }
        
        # determine which column of the strategy to select for j
        if (is.null(history_j)) {
          col_j <- 1L
        } else {
          h_j <- length(history_j-1)
          col_j <- 1 + (4^h_j - 1)/3 + sum((history_j-1L) * 4^(0:(h_j-1)))
        }
        
        
        # Determine cooperation
        decision_i  <- strat_i[col_i]
        decision_j  <- strat_j[col_j]
        
        # add to cooperation decisions 
        coop_i      <- coop_i + decision_i
        coop_j      <- coop_j + decision_j
        
        # update past history
        history_i   <- push_history(1L + decision_i + decision_j * 2, # 1 = DD, 2 = CD, 3 = DC, 4 = CC
                                    history_i,
                                    m)
        history_j   <- push_history(1L +  decision_j + decision_i * 2, # 1 = DD, 2 = CD, 3 = DC, 4 = CC
                                    history_j,
                                    m)
      }
      
      # Aggregate fitness changes
      agents$fitness[i] <- agents$fitness[i] - model$c * coop_i + model$b * coop_j
      agents$fitness[j] <- agents$fitness[j] - model$c * coop_j + model$b * coop_i
    }
  }
  agents$fitness <- agents$fitness / ((n-1) * rounds)
  
  # Save back and return
  world$agents <- agents
  world
}

run_step_fast <- function(world, model) {
  # draw random number of rounds
  rounds <- if (diff(model$expected_rounds)==0) model$expected_rounds[1]
    else sample(model$expected_rounds[1]:model$expected_rounds[2], 1L)
  
  # extract variables
  agents      <- world$agents
  strat_mat   <- world$strat_mat
  trans_mat   <- world$trans_mat
  n           <- nrow(agents)
  m           <- model$memory
  
  # Loop over unique pairs
  for (i in 1:(n-1L)) {
    for (j in (i+1L):n) {
      
      # get their strategies
      strat_i <- strat_mat[i, ]
      strat_j <- strat_mat[j, ]
      
      # how often i/j cooperated over ALL rounds
      coop_i  <- coop_j  <- 0   
      # the history of their interaction AS STATE
      state_i <- state_j <- 1L       # 1 = “first”
      
      for (r in seq_len(rounds)) {
        
        decision_i <- strat_i[ state_i ]
        decision_j <- strat_j[ state_j ]
        
        coop_i <- coop_i + decision_i
        coop_j <- coop_j + decision_j
        
        # 1 = DD , 2 = CD , 3 = DC , 4 = CC
        state_i  <- trans_mat[state_i,  decision_i + 2L * decision_j + 1L]
        state_j  <- trans_mat[state_j,  decision_j + 2L * decision_i + 1L]
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
  n_mutate  <- round(model$mu_rnd * n_update)
  mutators  <- sample(changers, n_mutate)
  imitators <- setdiff(changers, mutators)
  
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
      n_agents     = n(),
      mean_fitness = mean(fitness),
      .groups = "drop"
    ) |> 
    arrange(desc(mean_fitness)) |> 
    slice_head(n = top_n)          # keep only the top ⬆️ rows for display
}


run_sequence <- function(model) {
  # 1) Initialization
  world <- init_world(model)
  
  # 2) prepare data storage
  avg_fitness_series   <- integer(0)
  top10_series <- list()
  
  # 3) Iterate
  for (t in seq_len(model$max_t)) {
    # Perform one step (and update metrics inside world)
    world <- run_step_fast_cpp(world, model)
    
    # Store the data from world
    avg_fitness_series[t] <- mean(world$agents$fitness)
    top10_series[[t]]     <- summarise_strategies(world, 10)
    
    world <- update_agents(world, model)
  }
  
  df_top10 <- bind_rows(top10_series, .id = "step") |>
    mutate(step = as.integer(step))   
  
  # 3) Return final state and summary
  list(
    final_world        = world,
    avg_fitness_series = avg_fitness_series,
    top10              = df_top10
  )
}
