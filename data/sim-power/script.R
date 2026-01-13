library(tidyverse)
library(randomizr)
library(lme4)
library(lmerTest)
library(sandwich)
library(lmtest)

sim_data <- function(model) {

  # 1) build the base grid
  df <- expand_grid(
    school  = factor(paste0("School ", 1:model$schools)),
    class   = 1:model$classes,
    student = 1:model$n_per_class,
    randomization = model$randomization
  )
  
  # 2) apply random treatment design
  treatment <- switch (model$randomization,
    "simple"                  = simple_ra(N=nrow(df), prob = model$prob),
    "complete"                = complete_ra(N=nrow(df), prob = model$prob),
    "block(school)"           = block_ra(blocks=df$school, prob = model$prob),
    "block(class)"            = block_ra(blocks=df$class, prob = model$prob),
    "cluster(school)"         = cluster_ra(clusters=df$school, prob = model$prob),
    "cluster(class)"          = cluster_ra(clusters=df$class, prob = model$prob),
    "cluster(school, class)"  = cluster_ra(clusters=interaction(df$school, df$class, drop = TRUE), prob = model$prob),
  )
  
  # 3) calculate Y
  df |> 
    group_by(school) |> 
    mutate(
      u_school = rnorm(1, mean = 0, sd   = model$school_sd)
    ) |> 
    ungroup() |> 
    mutate(
      treatment = treatment,
      Y0 = rnorm(n(), mean = model$mean + u_school, sd = model$sd), # mean + u_school + random noise (sd)
      Y1 = Y0 + model$effect_treatment * treatment + rnorm(n())    # treatment effect + random error
    ) 
}

# simple regression, similar to t-test
run_test1 <- function(df) {
  fit <- lm(Y1 ~ treatment, data = df)
  pval <- summary(fit)$coefficients["treatment", "Pr(>|t|)"]
  pval < 0.05
}

run_test2 <- function(df) {
  fit <- "HIER KÖNNTE DEINE MODELL STEHEN"
  pval <- summary(fit)$coefficients["treatment", "Pr(>|t|)"]
  pval < 0.05
}

run_test3 <- function(df) {
  fit <- lm(Y1 ~ treatment, data = df)
  vc <- vcovCL(fit, cluster = ~ school)
  pval <- coeftest(fit, vcov = vc)["treatment", "Pr(>|t|)"]
  pval < 0.05
}

run_sim <- function(model, test) {
  df <- sim_data(model = model)
  stat <- switch(test,
                 "1" = run_test1(df),
                 "2" = run_test2(df),
                 "3" = run_test3(df)
          )
  
  return(stat)
}

# Function to get estimated power + binomial CI
get_power <- function(
    model,
    test = 1,
    replications = 1000,
    seed = 123
) {
  set.seed(seed)
  results <- replicate(replications, run_sim(model, test))
  sig_results <- sum(results)
  tibble(
    test     = test,
    power    = sig_results / replications,
  )
}

# 2.1

model <- list(
  # Y 
  mean = 50,
  sd = 10,
  effect_treatment = 5.5,
  school_sd        = 0,
  # design
  schools = 1,
  classes = 1,
  n_per_class = 100,
  prob = 0.5,
  randomization = "simple"
) 

# Should not be more than 4000
70 * model$prob * model$n_per_class + 10 * (1 - model$prob) * model$n_per_class 

get_power(model, test = 1, replications = 1000)

# 2.2

get_power(model, test = 2, replications = 1000)


# 3.1

model <- list(
  # Y 
  mean = 50,
  sd = 10,
  effect_treatment = 5.5,
  school_sd        = 0,
  # design
  schools = 12,
  classes = 1,
  n_per_class = 40,
  prob = 0.5,
  randomization = "cluster(school)"
) 

get_power(model, test = 3, replications = 1000)

