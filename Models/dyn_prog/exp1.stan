data {
  int<lower=0> T; // Number of trials
  int<lower=0> S; // Number of states
  int<lower=0> N; // Number of participants
  int<lower=0,upper=S> state[T, N]; // State matrix
  int<lower=0,upper=1> choice[T, N]; // Choice matrix
  matrix[S,S] transition_0; // Transition probabilities given choice 0
  matrix[S,S] transition_1; // Transition probabilities given choice 1
}

parameters {
  vector[S] reward; // Reward for each state
  real<lower=0> mu_costH; // Mean cost of high effort
  real<lower=0> mu_costL; // Mean cost of low effort
  real<lower=0> sigma_costH; // Standard deviation of costH across participants
  real<lower=0> sigma_costL; // Standard deviation of costL across participants
  vector<lower=0>[N] costH; // Cost of high effort for each participant
  vector<lower=0>[N] costL; // Cost of low effort for each participant
  real<lower=0,upper=1> beta; // Discount factor
}

transformed parameters {
  matrix[T,S] value; // Expected value of each state

  // Initialize the values for the last round
  for (k in 1:S) {
    for (n in 1:N) {
      value[T,k] = reward[k] - (choice[T, n]==1 ? costH[n] : costL[n]);
    }
  }

  // Backwards induction
  for (t in (T-1):1) {
    for (k in 1:S) {
      for (n in 1:N) {
        real ev0 = reward[k] - costL[n] + beta * dot_product(transition_0[k], value[t+1]);
        real ev1 = reward[k] - costH[n] + beta * dot_product(transition_1[k], value[t+1]);
        value[t,k] = log_sum_exp(ev0, ev1);
      }
    }
  }
}

model {
  // Priors
  mu_costH ~ normal(0, 10);
  mu_costL ~ normal(0, 10);
  sigma_costH ~ cauchy(0, 5);
  sigma_costL ~ cauchy(0, 5);
  costH ~ normal(mu_costH, sigma_costH);
  costL ~ normal(mu_costL, sigma_costL);

  for (t in 1:(T-1)) {
    for (n in 1:N) {
      real ev0 = reward[state[t, n]] - costL[n] + beta * dot_product(transition_0[state[t, n]], value[t+1]);
      real ev1 = reward[state[t, n]] - costH[n] + beta * dot_product(transition_1[state[t, n]], value[t+1]);
      choice[t, n] ~ bernoulli_logit(ev1 - ev0);
    }
  }
}
