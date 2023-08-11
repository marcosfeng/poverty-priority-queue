functions {
  real backward_induction(array[] int slices_n, int start, int end, int N,
                          int T, int K, int P, array[,] int state,
                          array[,] int choice, matrix effort, matrix reward,
                          array[] matrix transition_probs, array[] real beta,
                          array[] real costH, array[] int participants) {
    real result = 0;
    array[2] matrix[T, K] EVs;
    vector[T] alpha = rep_vector(0, T);
    
    for (n in start : end) {
      real common_cost = costH[participants[n]];
      matrix[2, T] b = beta[participants[n]] * rep_matrix(1, 2, T);
      matrix[T, 2] logit_probs;
      
      // Calculate the EV for the last round
      EVs[1, T,  : ] = (reward[ : , 1] - effort[ : , 1] * common_cost)';
      EVs[2, T,  : ] = (reward[ : , 2] - effort[ : , 2] * common_cost)';
      // Loop through the time and states, calculating the EVs for each choice
      for (t in 1 : (T - 1)) {
        EVs[1, T - t,  : ] = (transition_probs[1] * (EVs[1, T - t + 1,  : ]')
                              + reward[ : , 1] - effort[ : , 1] * common_cost)';
        EVs[2, T - t,  : ] = (transition_probs[2] * (EVs[2, T - t + 1,  : ]')
                              + reward[ : , 2] - effort[ : , 2] * common_cost)';
      }
      for (t in 1 : T) {
        int s = state[n, t]; // Current state
        logit_probs[t, 1] = EVs[1, t, s]; // EV for choice 1
        logit_probs[t, 2] = EVs[2, t, s]; // EV for choice 2
      }
      result += categorical_logit_glm_lpmf(choice[n] | logit_probs, alpha, b);
    }
    return result;
  }
}
data {
  int<lower=1> N; // number of blocks
  int<lower=1> K; // number of states
  int<lower=1> T; // number of trials
  int<lower=1> P; // number of participants
  array[N, T] int<lower=1, upper=K> state; // state matrix
  array[N, T] int<lower=1, upper=2> choice; // choice matrix
  matrix<lower=0, upper=1>[K, 2] effort; // effort matrix
  matrix<lower=0, upper=2>[K, 2] reward; // reward matrix
  array[2] matrix[K, K] transition_probs; // transition probability matrix
  array[N] int<lower=1, upper=P> participants; // participant identifiers
  int<lower=1> grainsize; // Grainsize for parallelization
  array[N] int slices_n; // Slices for parallelization
}
parameters {
  real<lower=0.001, upper=2> costH_mu; // mean cost for high effort
  real<lower=0> beta_mu; // mean beta
  
  real<lower=0> costH_sigma; // standard deviation of costH
  real<lower=0> beta_sigma; // standard deviation of beta
  
  array[P] real<lower=0, upper=2> costH; // cost for high effort for each participant
  array[P] real<lower=0> beta; // beta for each participant
}
model {
  costH_mu ~ uniform(0.001, 2); // prior for mean of costH
  beta_mu ~ exponential(0.25); // prior for mean of beta
  
  costH_sigma ~ exponential(1); // prior for standard deviation of costH
  beta_sigma ~ exponential(1); // prior for standard deviation of beta
  
  costH ~ normal(costH_mu, costH_sigma);
  beta ~ normal(beta_mu, beta_sigma);
  
  target += reduce_sum(backward_induction, slices_n, grainsize, N, T, K, P,
                       state, choice, effort, reward, transition_probs, beta,
                       costH, participants);
}
