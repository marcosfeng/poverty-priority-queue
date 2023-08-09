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
  real<lower=0.5, upper=1> gamma; // discount factor for each participant
  real<lower=0> costH; // cost MULTIPLIER for high effort for each participant
  real<lower=0> costL; // cost for low effort for each participant
  real<lower=0> beta; // beta for each participant
}
model {
  gamma ~ beta(10, 2); // prior for mean of gamma
  costH ~ exponential(1); // prior for mean of costH
  costL ~ exponential(1); // prior for mean of costL
  beta ~ exponential(0.25); // prior for mean of beta

  // EV for making a choice (1 or 2) in state K at time T
  array[2] matrix[T, K] EVs; 

  profile("backward_induction") {
    // Calculate the EV for the last round
    for (k in 1:K) {
      EVs[1, T, k] = reward[k, 1] - ((effort[k,1] != 2) ? ((effort[k,1] == 1) ? costH*costL : costL) : 0);
      EVs[2, T, k] = reward[k, 2] - ((effort[k,2] != 2) ? ((effort[k,2] == 1) ? costH*costL : costL) : 0);
    }

    // Loop through the time and states, calculating the EVs for each choice
    for (t in 1:(T-1)) {
      for (k in 1:K) {
        EVs[1, T-t, k] = dot_product(transition_probs[1, k, :], to_vector(EVs[1, T-t+1, :]) * gamma) + reward[k, 1] - ((effort[k,1] != 2) ? ((effort[k,1] == 1) ? costH*costL : costL) : 0);
        EVs[2, T-t, k] = dot_product(transition_probs[2, k, :], to_vector(EVs[2, T-t+1, :]) * gamma) + reward[k, 2] - ((effort[k,2] != 2) ? ((effort[k,2] == 1) ? costH*costL : costL) : 0);
      }
    }
  }
  
  profile("vectorized_choice_sampling") {
    vector[T] alpha = rep_vector(0, T);
    matrix[2, T] b = beta * rep_matrix(1, 2, T);
    for (n in 1:N) {
      matrix[T, 2] logit_probs;
      for (t in 1:T) {
        int s = state[n, t]; // Current state
        logit_probs[t, 1] = EVs[1, t, s]; // EV for choice 1
        logit_probs[t, 2] = EVs[2, t, s]; // EV for choice 2
      }
      // array[T] int y = choice[n];
      target += categorical_logit_glm_lpmf(choice[n] | logit_probs, alpha, b);
    }
  }
}
