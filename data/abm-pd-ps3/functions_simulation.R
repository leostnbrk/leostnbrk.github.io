##################################################################
#                                                                #
# Functions for ...                                              #
# running a complete replication or simulation                   #
#                                                                # 
# Version PD-3                                                   #
#                                                                #
##################################################################

library(parallel)
source("functions_data.R")

run_replications <- function(model,
                             replications = 10,
                             master_seed  = 42,
                             ncores       = parallel::detectCores() - 1
) {
  
  # 1) run the replications either sequential or parallel
  if (ncores == 1) {
    # run the sequences sequential
    # 1.1a) prepare a `results` vector and load functions
    results <- vector("list", replications)
    source("functions_sequence.R")
    
    # 1.1b) run the replications
    for (r in seq_len(replications)) {
      model$seed <- master_seed + r
      results[[r]] <- run_sequence(model)
    }
  } else { 
    # run the sequences parallel
    # 1.2a) start cluster and ensure it’s stopped on exit
    cl <- parallel::makePSOCKcluster(ncores)
    
    # load the functions needed for 
    parallel::clusterEvalQ(cl, {
      source("functions_sequence.R")
    })
    
    # export your model object (functions are already loaded)
    parallel::clusterExport(cl, c("model", "master_seed"), envir = environment())
    
    ## 1.2b) run all replications in parallel
    results <- parallel::parLapply(
      cl,
      seq_len(replications),
      fun = function(r) {
        model$seed <- master_seed + r
        run_sequence(model)
      }
    )
    
    stopCluster(cl) 
  }
  
  # 2) extract data
  get_data_replication(results)
}

run_simulation <- function(simulation){
  # 1) load the baseline model
  model <- load_model(simulation$model)
  
  # 2) prepare a list to collect each model’s result
  results <- vector("list", length = nrow(simulation$tbl_parameter))
  
  # 3) run all replications for all models
  for (i in seq_len(nrow(simulation$tbl_parameter))) {
    # fetch the temporary parameters
    tmp_parameter <- simulation$tbl_parameter |> 
      slice(i) |>                           # select the i-th row 
      as.list() 
    
    # check if a variable is a vector like c(2,2) and unwrap these list-columns 
    tmp_parameter_replace <- lapply(tmp_parameter,
                                     function(x) if (is.list(x) && length(x) == 1) x[[1]] else x
                                    )
    
    # replace the baseline model parameters with the temporary parameters
    tmp_model <- modifyList(model, tmp_parameter_replace)
    
    # run the replications with this temorary model
    result <- run_replications(model        = tmp_model, 
                               replications = simulation$replications,
                               master_seed  = simulation$master_seed,
                               ncores       = min(simulation$max_cores, parallel::detectCores()))
    
    # now we add the model name and the parameters that are changed
    # this function add it to df_process and df_final
    results[[i]] <- result %>%
      map(~ .x %>%
            mutate(
              model        = tmp_model$name,
              master_seed  = simulation$master_seed,
              replications = simulation$replications,
              !!!tmp_parameter
            )
      )
  }
  
  get_data_simulation(results)
}