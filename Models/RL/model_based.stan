data {
  int<lower=1> N;  // number of trials
  int<lower=1> K;  // number of actions
  int<lower=1,upper=K> a[N,P];  // chosen action for each participant
  real r[N,P];  // received reward for each participant
  int<lower=1> P;  // number of participants
}
parameters {
  matrix[K,K] T[P];  // estimated transition matrix for each participant
  matrix[K,P] R;  // estimated reward function for each participant
  vector<lower=0>[P] alpha;  // learning rates for each participant
  vector<lower=0>[P] beta;  // inverse temperatures for each participant
  vector<lower=0,upper=1>[P] gamma;  // discount factors for each participant
  real<lower=0> mu_alpha;  // population-level mean of alpha
  real<lower=0> mu_beta;  // population-level mean of beta
  real<lower=0> mu_gamma;  // population-level mean of gamma
  real<lower=0> sigma_alpha;  // population-level standard deviation of alpha
  real<lower=0> sigma_beta;  // population-level standard deviation of beta
  real<lower=0> sigma_gamma;  // population-level standard deviation of gamma
}
model {
  matrix[N,P] p;
  mu_alpha ~ beta(1, 1);  // hyperprior on population-level mean of alpha
  mu_beta ~ gamma(1, 1);  // hyperprior on population-level mean of beta
  mu_gamma ~ beta(0.5, 0.5);  // hyperprior on population-level mean of gamma
  sigma_alpha ~ cauchy(0, 1);  // hyperprior on population-level standard deviation of alpha
  sigma_beta ~ cauchy(0, 1);  // hyperprior on population-level standard deviation of beta
  sigma_gamma ~ cauchy(0, 1);  // hyperprior on population-level standard deviation of gamma
  alpha ~ normal(mu_alpha, sigma_alpha);  // prior on alphas
  beta ~ normal(mu_beta, sigma_beta);  // prior on betas
  gamma ~ normal(mu_gamma, sigma_gamma);  // prior on gammas
  for (participant in 1:P) {
    T[participant] ~ dirichlet(rep_vector(1, K));  // prior on transition probabilities
    R[:,participant] ~ normal(0, 1);  // prior on rewards
  }

  for (n in 2:N) {
    for (participant in 1:P) {
      // Update estimated transition and reward functions
      T[participant][a[n-1,participant], :] += alpha[participant] * ((a[n] == 1:N) - T[participant][a[n-1,participant], :]);
      R[a[n-1,participant],participant] += alpha[participant] * (r[n-1,participant] - R[a[n-1,participant],participant]);
      // Compute action values via dynamic programming
      vector[K] Q = R[:,participant] + gamma[participant] * T[participant]' * Q;
      p[n,participant] = beta[participant] * Q[a[n-1,participant]];
    }
  }
  for (participant in 1:P) {
    a[:,participant] ~ categorical_logit(p[:,participant]);
  }
}
