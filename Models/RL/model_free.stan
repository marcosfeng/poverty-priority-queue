data {
  int<lower=1> N;  // number of trials
  int<lower=1> K;  // number of actions
  int<lower=1,upper=K> a[N,P];  // chosen action for each participant
  real r[N,P];  // received reward for each participant
  int<lower=1> P;  // number of participants
}
parameters {
  matrix[K,P] q;  // action values for each participant
  vector<lower=0>[P] alpha;  // learning rates for each participant
  vector<lower=0>[P] beta;  // inverse temperatures for each participant
  real<lower=0> mu_alpha;  // population-level mean of alpha
  real<lower=0> mu_beta;  // population-level mean of beta
  real<lower=0> sigma_alpha;  // population-level standard deviation of alpha
  real<lower=0> sigma_beta;  // population-level standard deviation of beta
}
model {
  matrix[N,P] p;
  mu_alpha ~ beta(1, 1);  // hyperprior on population-level mean of alpha
  mu_beta ~ gamma(1, 1);  // hyperprior on population-level mean of beta
  sigma_alpha ~ cauchy(0, 1);  // hyperprior on population-level standard deviation of alpha
  sigma_beta ~ cauchy(0, 1);  // hyperprior on population-level standard deviation of beta
  alpha ~ normal(mu_alpha, sigma_alpha);  // prior on alphas
  beta ~ normal(mu_beta, sigma_beta);  // prior on betas
  to_vector(q) ~ normal(0, 1);  // prior on action values
  
  for (n in 2:N) {
    for (participant in 1:P) {
      p[n,participant] = beta[participant] * q[a[n-1,participant],participant];
      q[a[n-1,participant],participant] += alpha[participant] * (r[n-1,participant] - q[a[n-1,participant],participant]);
    }
  }
  for (participant in 1:P) {
    a[:,participant] ~ categorical_logit(p[:,participant]);
  }
}
