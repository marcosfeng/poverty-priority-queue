data {
  int<lower=1> N; // number of blocks
  int<lower=1> K; // number of states
  int<lower=1> T; // number of trials
  int<lower=1, upper=K> state[N, T]; // state matrix
  int<lower=1, upper=2> choice[N, T]; // choice matrix
  int<lower=0, upper=1> effort[K, 2]; // effort matrix
  int<lower=1, upper=2> reward[K, 2]; // reward matrix
  matrix[K, K] transition_probs[2]; // transition probability matrix
  int<lower=1> P; // number of participants
  int<lower=1, upper=P> participants[N]; // participant identifiers
}
parameters {
  real<lower=0, upper=1> gamma_mu; // mean of gamma
  real<lower=0> gamma_sigma; // standard deviation of gamma
  real<lower=0, upper=1> gamma[P]; // discount factor for each participant

  real<lower=0> costH_mu; // mean of costH
  real<lower=0> costH_sigma; // standard deviation of costH
  real<lower=0> costH[P]; // cost for high effort for each participant

  real<lower=0> costL_mu; // mean of costL
  real<lower=0> costL_sigma; // standard deviation of costL
  real<lower=0> costL[P]; // cost for low effort for each participant

  real<lower=0> beta_mu; // mean of beta
  real<lower=0> beta_sigma; // standard deviation of beta
  real<lower=0> beta[P]; // inverse temperature for each participant
}
model {
  gamma_mu ~ beta(1, 1); // prior for mean of gamma
  gamma_sigma ~ cauchy(0, 2.5); // prior for standard deviation of gamma
  for (p in 1:P) {
    gamma[p] ~ beta(gamma_mu, gamma_sigma); // group-level prior for gamma
  }

  costH_mu ~ exponential(1); // prior for mean of costH
  costH_sigma ~ cauchy(0, 2.5); // prior for standard deviation of costH
  for (p in 1:P) {
    costH[p] ~ normal(costH_mu, costH_sigma); // group-level prior for costH
  }

  costL_mu ~ exponential(1); // prior for mean of costL
  costL_sigma ~ cauchy(0, 2.5); // prior for standard deviation of costL
  for (p in 1:P) {
    costL[p] ~ normal(costL_mu, costL_sigma); // group-level prior for costL
  }

  beta_mu ~ exponential(0.5); // prior for mean of beta
  beta_sigma ~ cauchy(0, 2.5); // prior for standard deviation of beta
  for (p in 1:P) {
    beta[p] ~ normal(beta_mu, beta_sigma); // group-level prior for beta
  }

  for (n in 1:N) {
    int p = participants[n];
    // Initialize the expected value vector for the last round
    vector[K] EV_last_round;
    real EV_choice_1_last_round;
    real EV_choice_2_last_round;
    for (k in 1:K) {
      EV_choice_1_last_round = reward[k, 1] - ((effort[k,1] == 1) ? costH[p] : costL[p]);
      EV_choice_2_last_round = reward[k, 2] - ((effort[k,2] == 1) ? costH[p] : costL[p]);
      EV_last_round[k] = (EV_choice_1_last_round > EV_choice_2_last_round) ? EV_choice_1_last_round : EV_choice_2_last_round;
    }
    EV_choice_1_last_round = reward[state[n,T], 1] - ((effort[state[n,T],1] == 1) ? costH[p] : costL[p]);
    EV_choice_2_last_round = reward[state[n,T], 2] - ((effort[state[n,T],2] == 1) ? costH[p] : costL[p]);
    choice[n, T] ~ categorical_logit(beta[p] * to_vector([EV_choice_1_last_round, EV_choice_2_last_round]));
    
    // Loop back from the last round to the first
    for (t in 1:(T-1)) {
      vector[K] EV_current_round;
      for (k in 1:K) {
        real EV_choice_1 = dot_product(transition_probs[1, k, :], EV_last_round * gamma[p]) + reward[k, 1] - ((effort[k,1] == 1) ? costH[p] : costL[p]);
        real EV_choice_2 = dot_product(transition_probs[2, k, :], EV_last_round * gamma[p]) + reward[k, 2] - ((effort[k,2] == 1) ? costH[p] : costL[p]);
        EV_current_round[k] = (EV_choice_1 > EV_choice_2) ? EV_choice_1 : EV_choice_2;
      }
      EV_choice_1_last_round = reward[state[n,T-t], 1] - ((effort[state[n,T-t],1] == 1) ? costH[p] : costL[p]);
      EV_choice_2_last_round = reward[state[n,T-t], 2] - ((effort[state[n,T-t],2] == 1) ? costH[p] : costL[p]);
      choice[n, T-t] ~ categorical_logit(beta[p] * to_vector([EV_choice_1_last_round, EV_choice_2_last_round]));
      EV_last_round = EV_current_round;
    }
  }
}
