data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=2> nOpt;
  int<lower=1, upper=T> Tsubj[N];
  int<lower=0, upper=nOpt> choice[N, T]; //left or right?
  int<lower=0, upper=80> opt_st[N, T]; //option-state: an easy way to map the choices for choice prob calculation
  int value_lookup[80];
  int state_lookup[52, nOpt];
  matrix<lower=0, upper = 1>[80, 52] prob_weight;
  int<lower=0, upper=80> counterpart[80];
  // real outcome[N, T];  // no lower and upper bounds
}
transformed data {
  vector[nOpt] initV;  // initial values for EV
  initV = rep_vector(0.0, nOpt);
}
parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[2] mu_pr;
  vector<lower=0>[2] sigma;

  // Subject-level raw parameters (for transformation from hyper to subj parameter)
  // vector[N] tau_pr;    // inverse temperature
  vector[N] CL_pr;  // cost_low
  vector[N] CH_pr;    // cost_high
}
transformed parameters {
  // subject-level parameters
  // vector<lower=0>[N] tau; // take away the restriction on alpha
  vector[N] costL;
  vector[N] costH;

  for (i in 1:N) {
    //tau[i]   = Phi_approx(mu_pr[1]  + sigma[1]  * tau_pr[i]); //approx Normal CDF + noise
    costL[i] = Phi_approx(mu_pr[1] + sigma[1] * CL_pr[i]);
    costH[i] = Phi_approx(mu_pr[2]  + sigma[2]  * CH_pr[i]); //approx Normal CDF + noise
  }
}
model {
  // Hyperparameters
  mu_pr  ~ normal(0, 1);
  sigma ~ normal(0, 0.2);

  // individual parameters
  //tau_pr   ~ normal(0, 1);
  CL_pr ~ normal(0, 1);
  CH_pr   ~ normal(0, 1);

  // subject loop and trial loop
  for (i in 1:N) {
    vector[nOpt] ev; // expected value
    vector[4] value; // vector of value option, lookup table
    matrix[80, Tsubj[i]] ch; 
    matrix[52, Tsubj[i]] st;
    int round_back; // backwards counter for induction
    real weighted_value;
    
    ev = initV;

    // Declaring values for each option, lookup table (make it loop later)
    value[1] = 2 - costH[i];
    value[2] = 1 - costH[i];
    value[3] = 1 - costL[i];
    value[4] = 2 - costL[i];


    // Backwards induction
    //  fill in column in "ch" with the values
    for(option in 1:80) {
      // first column is choice: find in tasks table the value of that choice
      // lookup is a vector that tells you which cost correspondends to each choice
      ch[option, Tsubj[i]] = value[value_lookup[option]];
    }
    //    Create state values for each round
    // state_lookup tells you which choice|state maps onto which state
    // for each state in "S" choose the highest value in "ch" and create another column with the values
    for(state in 1:52) {
      if (ch[state_lookup[state,1], Tsubj[i]] >= ch[state_lookup[state,2], Tsubj[i]]) {
        st[state, Tsubj[i]] = ch[state_lookup[state,1], Tsubj[i]];
      } else if (ch[state_lookup[state,1], Tsubj[i]] < ch[state_lookup[state,2], Tsubj[i]]) {
        st[state, Tsubj[i]] = ch[state_lookup[state,2], Tsubj[i]];
      }
    }
        // compute action probabilities
        ev[1] = ch[opt_st[i, Tsubj[i]], Tsubj[i]];
        ev[2] = ch[counterpart[opt_st[i, Tsubj[i]]], Tsubj[i]];
        choice[i, Tsubj[i]] ~ categorical_logit(ev);
        
        for (t in 1:(Tsubj[i]-1)) {
          round_back = Tsubj[i] - t;
          for(option in 1:80) {
            // use action probabilities
            weighted_value = dot_product(prob_weight[option], col(st, (round_back + 1)) );
            ch[option, round_back] = value[value_lookup[option]] + weighted_value;
          }
          for(state in 1:52) {
            if (ch[state_lookup[state,1], round_back] >= ch[state_lookup[state,2], round_back]) {
              st[state, round_back] = ch[state_lookup[state,1], round_back];
            } else if (ch[state_lookup[state,1], round_back] < ch[state_lookup[state,2], round_back]) {
              st[state, round_back] = ch[state_lookup[state,2], round_back];
            }
          }
          // compute action probabilities
          ev[1] = ch[opt_st[i, round_back], round_back];
          ev[2] = ch[counterpart[opt_st[i, round_back]], round_back];
          choice[i, round_back] ~ categorical_logit(ev);
        }
  }
}
generated quantities {
  // For group level parameters
  real mu_CL; // take away the restriction on alpha
  //real<lower=0> mu_tau; // take away the restriction on beta
  real mu_CH;

  // For log likelihood calculation
  real log_lik[N];

  // For posterior predictive check
  real y_pred[N, T];

  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i, t] = -1;
    }
  }

  mu_CL   = Phi_approx(mu_pr[1]);
  //mu_tau = Phi_approx(mu_pr[2]) * 5;
  mu_CH   = Phi_approx(mu_pr[2])  * 2;

  { // local section, this saves time and space
    for (i in 1:N) {
      vector[nOpt] ev; // expected value
      vector[8] value; // vector of value option, lookup table
      matrix[80, Tsubj[i]] ch; 
      matrix[52, Tsubj[i]] st;
      int round_back; // backwards counter for induction
      real weighted_value;
      
      // Initialize values
      ev = initV;
      value[1] = 2 - costH[i];
      value[2] = 1 - costH[i];
      value[3] = 1 - costL[i];
      value[4] = 2 - costL[i];

      log_lik[i] = 0;
      
      // first round of backward induction
      for(option in 1:80) {
        // first column is choice: find in tasks table the value of that choice
        // lookup is a vector that tells you which cost correspondends to each choice
        ch[option, Tsubj[i]] = value[value_lookup[option]];
      }
      //    Create state values for each round
      // state_lookup tells you which choice|state maps onto which state
      // for each state in "S" choose the highest value in "ch" and create another column with the values
      for(state in 1:52) {
        if (ch[state_lookup[state,1], Tsubj[i]] >= ch[state_lookup[state,2], Tsubj[i]]) {
          st[state, Tsubj[i]] = ch[state_lookup[state,1], Tsubj[i]];
        } else if (ch[state_lookup[state,1], Tsubj[i]] < ch[state_lookup[state,2], Tsubj[i]]) {
          st[state, Tsubj[i]] = ch[state_lookup[state,1], Tsubj[i]];
        }
      }
      // compute action probabilities
      ev[1] = ch[opt_st[i, Tsubj[i]], Tsubj[i]];
      ev[2] = ch[counterpart[opt_st[i, Tsubj[i]]], Tsubj[i]];
      log_lik[i] += categorical_logit_lpmf(choice[i, Tsubj[i]] | ev);
      y_pred[i, Tsubj[i]] = categorical_rng(softmax(ev));

      for (t in 1:(Tsubj[i]-1)) {
        round_back = Tsubj[i] - t;
        for(option in 1:80) {
          // use action probabilities
          weighted_value = dot_product(prob_weight[option], col(st, (round_back + 1)) );
          ch[option, round_back] = value[value_lookup[option]] + weighted_value;
        }
        for(state in 1:52) {
          if (ch[state_lookup[state,1], round_back] >= ch[state_lookup[state,2], round_back]) {
            st[state, round_back] = ch[state_lookup[state,1], round_back];
          } else if (ch[state_lookup[state,1], round_back] < ch[state_lookup[state,2], round_back]) {
            st[state, round_back] = ch[state_lookup[state,2], round_back];
          }
        }
        // compute action values
        ev[1] = ch[opt_st[i, round_back], round_back];
        ev[2] = ch[counterpart[opt_st[i, round_back]], round_back];

        // compute log likelihood of current trial
        log_lik[i] += categorical_logit_lpmf(choice[i, round_back] | ev);

        // generate posterior prediction for current trial
        y_pred[i, round_back] = categorical_rng(softmax(ev));
      }
    }
  }
}
