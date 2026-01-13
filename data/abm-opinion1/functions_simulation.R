##################################################################
#                                                                #
# Functions for ...                                              #
# Opinion Dynamics                                               #
#                                                                # 
# Version Opinion Dynamics 1                                     #
#                                                                #
##################################################################

library(parallel)
source("functions_data.R")
source("functions_calibration.R")
source("functions_models.R")

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

run_simulation <- function(simulation,
                           empirical_data = NULL){
  # 1) load the baseline model
  model <- load_model(simulation$model)
  
  # 2) prepare a list to collect each model’s result
  results <- vector("list", length = nrow(simulation$tbl_parameter))
  
  results_calibration <- if (!is.null(empirical_data)) vector("list", length = nrow(simulation$tbl_parameter)) else NULL
  
  # 3) run all replications for all models
  for (i in seq_len(nrow(simulation$tbl_parameter))) {
    cat(paste0("Combination: ", i, "\n"))
    
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
    
    if (!is.null(empirical_data)) {
      # pick whichever summary you compare (e.g. time-series)
      syn_df <- as.comparable_df(sim = result)
      emp_df <- as.comparable_df(emp = empirical_data)
      fit    <- get_measure_fit(empirical_data  = emp_df,
                                synthetic_data = syn_df)
      # store the single-row tibble of params + fit
      results_calibration[[i]] <- tibble(fit = fit,
                                         !!!tmp_parameter
                                         )
    }
  }
  
  out <- get_data_simulation(results)
  
  if (!is.null(empirical_data)) {
    out$df_calibration <- bind_rows(results_calibration)
  }
  
  return(out)
}

tbl_parameters_LHS <- function(parameters, s = 20, seed = NULL) {
  # for replication, allow for seeding
  if (!is.null(seed)) set.seed(seed)
  
  # 1) generate the LHS matrix
  LHS_matrix <- randomLHS(s , length(parameters))
  # make it a df
  tbl_parameters <- as.data.frame(LHS_matrix)
  # 2) transform values 
  for (i in seq_along(parameters)){
    if (parameters[[i]][[1]] ==   "qinteger") tbl_parameters[,i] <-   qinteger(tbl_parameters[,i], parameters[[i]][[2]], parameters[[i]][[3]])
    if (parameters[[i]][[1]] ==      "qnorm") tbl_parameters[,i] <-      qnorm(tbl_parameters[,i], parameters[[i]][[2]], parameters[[i]][[3]])
    if (parameters[[i]][[1]] ==    "qfactor") tbl_parameters[,i] <-    qfactor(tbl_parameters[,i], parameters[[i]][[2]])
    if (parameters[[i]][[1]] == "qdirichlet") tbl_parameters[,i] <- qdirichlet(tbl_parameters[,i], parameters[[i]][[2]])
    if (parameters[[i]][[1]] ==   "qnumeric") tbl_parameters[,i] <- parameters[[i]][[2]] + (tbl_parameters[,i] * parameters[[i]][[3]])
    if (parameters[[i]][[1]] ==     "static") tbl_parameters[,i] <- parameters[[i]][[2]]
    
  }
  # name them
  names(tbl_parameters) <- names(parameters)
  
  return(tbl_parameters)
}