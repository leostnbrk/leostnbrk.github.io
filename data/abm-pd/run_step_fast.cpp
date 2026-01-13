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
  NumericVector  fitness = agents["fitness"];    // will update in-place
  
  IntegerVector  exp_rd  = model["expected_rounds"]; // length-2 (lo, hi)
  double         b       = model["b"];
  double         c       = model["c"];
  
  const int nAgents = strat.nrow();

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
        int dec_i = static_cast<int>( strat(i, state_i - 1) ); // C++ index = state-1
        int dec_j = static_cast<int>( strat(j, state_j - 1) );
        
        coop_i += dec_i;
        coop_j += dec_j;
        
        // outcome seen from each side: 0 = DD, 1 = CD, 2 = DC, 3 = CC
        // in cpp vectors are from [0...n-1] instead of [1...n] 
        int outcome_i =  dec_i + 2 * dec_j;      // 0..3
        int outcome_j =  dec_j + 2 * dec_i;
        
        // follow the finite-state machine
        state_i = trans(state_i - 1, outcome_i) ;
        state_j = trans(state_j - 1, outcome_j) ;
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
