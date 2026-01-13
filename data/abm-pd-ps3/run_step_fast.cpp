// [[Rcpp::depends(Rcpp)]]
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
using namespace Rcpp;

// ---------------------------------------------------------------------
// Helper: draw a single integer in [lo, hi]  (fast, inline)
// ---------------------------------------------------------------------
inline int one_uniform_int(int lo, int hi)
{
  if (lo == hi) return lo;
  return lo + static_cast<int>(R::runif(0.0, 1.0) * (hi - lo + 1));
}

// ---------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::List run_step_pC_err_cpp(Rcpp::List world, Rcpp::List model)
{
  // ---- pull everything we need --------------------------------------
  NumericMatrix strat = world["strat_mat"];      // rows = agents, cell = P(C)
  IntegerMatrix trans = world["trans_mat"];      // state-transition table
  DataFrame     agents = world["agents"];        // id + fitness column
  
  const int nAgents = strat.nrow();
  NumericVector fitness(nAgents, 0.0);
  
  IntegerVector exp_rd = model["expected_rounds"];  // length-2 (lo, hi)
  double        b          = model["b"];
  double        c          = model["c"];
  double        errorRate  = model["error"];        // execution error  (0–1)
  double        noiseRate  = model["noise"];        // perception noise (0–1)
  
  // optional: `pC_levels` tells us whether the strategy matrix is binary
  int pC_levels = model.containsElementNamed("pC_levels") ?
  as<int>(model["pC_levels"]) : 0;
  
  // ---- decide once how many rounds this generation will last ---------
  const int rounds = one_uniform_int(exp_rd[0], exp_rd[1]);
  
  // ---- FAST-PATH FLAGS  (constant for the whole run) -----------------
  const bool noError   = (errorRate == 0.0);
  const bool noNoise   = (noiseRate == 0.0);
  const bool binaryPC  = (pC_levels == 2);          // prob_C is always 0 or 1
  
  // ---- main pair-wise loop ------------------------------------------
  for (int i = 0; i < nAgents - 1; ++i) {
    for (int j = i + 1; j < nAgents; ++j) {
      
      int state_i = 1, state_j = 1;      // finite-state machine starts at 1
      int coop_i  = 0, coop_j  = 0;      // cooperation counters (executed)
      
      for (int r = 0; r < rounds; ++r) {
        
        // 1) intended moves -------------------------------------------
        double pCi = strat(i, state_i - 1);
        double pCj = strat(j, state_j - 1);
        
        int intended_i, intended_j;
        
        if (binaryPC) {
          // p is literally 0 or 1  →  cast is enough, no RNG
          intended_i = static_cast<int>(pCi);
          intended_j = static_cast<int>(pCj);
        } else {
          // general case  →  Bernoulli(p)
          intended_i = (R::runif(0.0, 1.0) < pCi) ? 1 : 0;
          intended_j = (R::runif(0.0, 1.0) < pCj) ? 1 : 0;
        }
        
        // 2) execution error ------------------------------------------
        int action_i, action_j;
        
        if (noError) {
          action_i = intended_i;
          action_j = intended_j;
        } else {
          action_i = (R::runif(0.0, 1.0) < errorRate)
          ? one_uniform_int(0, 1)
            : intended_i;
          action_j = (R::runif(0.0, 1.0) < errorRate)
            ? one_uniform_int(0, 1)
              : intended_j;
        }
        
        // 3) tally cooperation ---------------------------------------
        coop_i += action_i;
        coop_j += action_j;
        
        // 4) perception noise ----------------------------------------
        int percep_i_of_j, percep_j_of_i;
        
        if (noNoise) {
          percep_i_of_j = action_j;
          percep_j_of_i = action_i;
        } else {
          percep_i_of_j = (R::runif(0.0, 1.0) < noiseRate)
          ? one_uniform_int(0, 1)
            : action_j;
          percep_j_of_i = (R::runif(0.0, 1.0) < noiseRate)
            ? one_uniform_int(0, 1)
              : action_i;
        }
        
        // 5) finite-state machine update -----------------------------
        // outcome index 0..3 : 0 = DD, 1 = CD, 2 = DC, 3 = CC
        state_i = trans(state_i - 1, intended_i + 2 * percep_i_of_j);
        state_j = trans(state_j - 1, intended_j + 2 * percep_j_of_i);
      }
      
      // 6) accumulate fitness ----------------------------------------
      fitness[i] += -c * coop_i + b * coop_j;
      fitness[j] += -c * coop_j + b * coop_i;
    }
  }
  
  // ---- write results back to R --------------------------------------
  agents["fitness"] = fitness / (rounds * (nAgents - 1));
  world["agents"]   = agents;
  return world;
}

// -------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::List run_step_cpp(Rcpp::List world, Rcpp::List model)
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