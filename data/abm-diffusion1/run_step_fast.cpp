// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
using namespace Rcpp;

// -------------------------------------------------------------------------
  // Helper: draw a single integer from an inclusive range  [lo, hi]
// -------------------------------------------------------------------------
  inline int one_uniform_int(int lo, int hi)
{
  if (lo == hi) return lo;
  return lo + (R::runif(0.0, 1.0) * (hi - lo + 1));   // fast, no allocation
}

// -------------------------------------------------------------------------
  // [[Rcpp::export]]
Rcpp::List run_step_fast_cpp(Rcpp::List world, Rcpp::List model)
{
  // ----- pull everything we need out of the R lists ---------------------
  NumericMatrix  strat   = world["strat_mat"];   // rows = agents
  IntegerMatrix  trans   = world["trans_mat"];   // rows = states, cols = outcomes
  DataFrame      agents  = world["agents"];      // id + fitness
  
  const int nAgents = strat.nrow();
  NumericVector  fitness(nAgents, 0.0);
  
  IntegerVector  exp_rd  = model["expected_rounds"]; // length-2 (lo, hi)
  double         b       = model["b"];
  double         c       = model["c"];

  // ----- decide once how many rounds this “generation” will last --------
  const int rounds = one_uniform_int(exp_rd[0], exp_rd[1]);
  
  // ----- main pair-wise loop --------------------------------------------
  for (int i = 0; i < nAgents - 1; ++i) {
    for (int j = i + 1; j < nAgents; ++j) {
      
      int state_i = 1;
      int state_j = 1;
      int coop_i  = 0;
      int coop_j  = 0;
      
      for (int r = 0; r < rounds; ++r) {
        
        // strategies store 0/1 as *numeric*, so cast to int
        int intended_i = static_cast<int>(strat(i, state_i - 1));
        int intended_j = static_cast<int>(strat(j, state_j - 1));
        
        int action_i = intended_i;
        int action_j = intended_j;
        
        coop_i += action_i;
        coop_j += action_j;
        
        // perception noise: each agent may mis-perceive the partner’s move
        int perception_i_of_j = action_j;
        int perception_j_of_i = action_i;
        
        // follow the finite-state machine
        state_i = trans(state_i - 1, intended_i + 2 * perception_i_of_j);
        state_j = trans(state_j - 1, intended_j + 2 * perception_j_of_i);
      }
      
      // update fitness for the pair
      fitness[i] += -c * coop_i + b * coop_j;
      fitness[j] += -c * coop_j + b * coop_i;
    }
  }
  
  // ----- write the new fitness vector back into the data-frame & world ---
  agents["fitness"] = fitness / (rounds * (nAgents-1));
  world["agents"]   = agents;
  
  return world;
}
  
  // [[Rcpp::export]]
Rcpp::List run_step_rnd_err_cpp(Rcpp::List world, Rcpp::List model)
{
  // ----- pull everything we need out of the R lists ---------------------
  NumericMatrix  strat   = world["strat_mat"];   // rows = agents
  IntegerMatrix  trans   = world["trans_mat"];   // rows = states, cols = outcomes
  DataFrame      agents  = world["agents"];      // id + fitness
  
  const int nAgents = strat.nrow();
  NumericVector  fitness(nAgents, 0.0);
  
  IntegerVector  exp_rd  = model["expected_rounds"]; // length-2 (lo, hi)
  double         b       = model["b"];
  double         c       = model["c"];
  double errorRate       = model["error"];              // execution error  (0‒1)
  double noiseRate       = model["noise"];              // perception noise (0‒1)
  
  // ----- decide once how many rounds this “generation” will last --------
  const int rounds = one_uniform_int(exp_rd[0], exp_rd[1]);
  
  // ----- main pair-wise loop --------------------------------------------
  for (int i = 0; i < nAgents - 1; ++i) {
    for (int j = i + 1; j < nAgents; ++j) {
      
      int state_i = 1;
      int state_j = 1;
      int coop_i  = 0;
      int coop_j  = 0;
      
      for (int r = 0; r < rounds; ++r) {
        
        // draw intended moves from the strategy probabilities
        double prob_C_i = strat(i, state_i - 1);          // P(cooperate) for i
        double prob_C_j = strat(j, state_j - 1);          // P(cooperate) for j
        
        // strategies store 0/1 as *numeric*, so cast to int
        int intended_i = (R::runif(0.0, 1.0) < prob_C_i) ? 1 : 0;  // 0 = D, 1 = C
        int intended_j = (R::runif(0.0, 1.0) < prob_C_j) ? 1 : 0;
        
        int action_i = (R::runif(0.0, 1.0) < errorRate)
          ? one_uniform_int(0, 1)
            : intended_i;
        int action_j = (R::runif(0.0, 1.0) < errorRate)
          ? one_uniform_int(0, 1)
            : intended_j;
        
        coop_i += action_i;
        coop_j += action_j;
        
        // perception noise: each agent may mis-perceive the partner’s move
        int perception_i_of_j = (R::runif(0.0, 1.0) < noiseRate)
          ? one_uniform_int(0, 1)
            : action_j;
        int perception_j_of_i = (R::runif(0.0, 1.0) < noiseRate)
          ? one_uniform_int(0, 1)
            : action_i;
        
        // follow the finite-state machine
        state_i = trans(state_i - 1, intended_i + 2 * perception_i_of_j);
        state_j = trans(state_j - 1, intended_j + 2 * perception_j_of_i);
      }
      
      // update fitness for the pair
      fitness[i] += -c * coop_i + b * coop_j;
      fitness[j] += -c * coop_j + b * coop_i;
    }
  }
  
  // ----- write the new fitness vector back into the data-frame & world ---
  agents["fitness"] = fitness / (rounds * (nAgents-1));
  world["agents"]   = agents;
  
  return world;
}