data {
  int<lower=1> N; // number of blocks
  int<lower=1> K; // number of states
  int<lower=1> T; // number of trials
  array[N, T] int<lower=1, upper=K> state; // state matrix
  array[N, T] int<lower=1, upper=2> choice; // choice matrix
  array[K, 2] int<lower=0, upper=2> effort; // effort matrix
  array[K, 2] int<lower=1, upper=2> reward; // reward matrix
  array[2] matrix[K, K] transition_probs; // transition probability matrix
  int<lower=1> P; // number of participants
  array[N] int<lower=1, upper=P> participants; // participant identifiers
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
  costH_mu ~ uniform(0.001,2); // prior for mean of costH
  beta_mu ~ exponential(0.25); // prior for mean of beta
  
  costH_sigma ~ exponential(1); // prior for standard deviation of costH
  beta_sigma ~ exponential(1); // prior for standard deviation of beta

  costH ~ normal(costH_mu, costH_sigma);
  beta ~ normal(beta_mu, beta_sigma);

  // EV for making a choice (1 or 2) in state K at time T
  array[2] matrix[T, K] EVs;
  print("beta: ", beta_mu, ", cost: ", costH_mu);
  profile("backward_induction") {
    for (n in 1:N) {
      real common_cost = costH[participants[n]];
      // Calculate the EV for the last round
      for (k in 1:K) {
        EVs[1, T, k] = reward[k, 1] - ((effort[k,1] == 1) ? common_cost : 0);
        EVs[2, T, k] = reward[k, 2] - ((effort[k,2] == 1) ? common_cost : 0);
      }
  
      // Loop through the time and states, calculating the EVs for each choice
      for (t in 1:(T-1)) {
        for (k in 1:K) {
          EVs[1, T-t, k] = dot_product(transition_probs[1, k, :], to_vector(EVs[1, T-t+1, :])) + reward[k, 1] - ((effort[k,1] == 1) ? costH[participants[n]] : 0);
          EVs[2, T-t, k] = dot_product(transition_probs[2, k, :], to_vector(EVs[2, T-t+1, :])) + reward[k, 2] - ((effort[k,2] == 1) ? costH[participants[n]] : 0);
        }
      }
    }
  }
  
  profile("vectorized_choice_sampling") {
    vector[T] alpha = rep_vector(0, T);
    for (n in 1:N) {
      matrix[2, T] b = beta[participants[n]] * rep_matrix(1, 2, T);
      matrix[T, 2] logit_probs;
      for (t in 1:T) {
        int s = state[n, t]; // Current state
        logit_probs[t, 1] = EVs[1, t, s]; // EV for choice 1
        logit_probs[t, 2] = EVs[2, t, s]; // EV for choice 2
      }
      target += categorical_logit_glm_lpmf(choice[n] | logit_probs, alpha, b);
    }
  }
}
