data {
  // Initialization and data
  int<lower=0> K; // number of polls
  int<lower=0> N; // number of states
  vector[K] reg_vec; // regressor (either beta or ddc)
  int<lower=1> T_poll; // number of poll types

  // Indicators
  int<lower=1, upper=T_poll> poll_type[K]; // indicator for type
  int<lower=1, upper=N> st[K]; // indicator for state

  // Additional specification
  int<lower=0, upper=1> use_laplace;
}

parameters {
  // Sigma Hierarchical
  real phi_0;
  vector[N] phi_st;
  vector[T_poll] phi_poll;

  // Mean Hierarchical
  real gamma_0;
  vector[N] gamma_st;
  vector[T_poll] gamma_poll;

  // Hyperparameters
  real<lower=0> sd_gamma_st; // state-level random effect on state
  real<lower=0> sd_gamma_poll;
  real<lower=0> sd_phi_st;
  real<lower=0> sd_phi_poll;
}

transformed parameters{
  matrix[N, T_poll] mu_reg_est;
  matrix<lower=0>[N, T_poll] sigma_reg_est;

  // Multi-level regression
  for (j in 1:T_poll) {
    for (i in 1:N) {
      mu_reg_est[i, j] = gamma_0 + gamma_st[i] + gamma_poll[j];
      sigma_reg_est[i, j] = exp(phi_0 + phi_st[i] + phi_poll[j]);
    }
  }
}

model {
  // Lkhd
  {
    vector[K] summands;
    if (use_laplace == 0) {
      for (k in 1:K) {
        summands[k] = normal_lpdf(reg_vec[k] | mu_reg_est[st[k], poll_type[k]], sigma_reg_est[st[k], poll_type[k]]);
      }
    } else {
      for (k in 1:K) {
        summands[k] = double_exponential_lpdf(reg_vec[k] | mu_reg_est[st[k], poll_type[k]], sigma_reg_est[st[k], poll_type[k]]);
      }
    }

    target += sum(summands);
  }

  // Hierarchical
  phi_st ~ normal(0, sd_phi_st);
  phi_poll ~ normal(0, sd_phi_poll);
  gamma_st ~ normal(0, sd_gamma_st);
  gamma_poll ~ normal(0, sd_gamma_poll);

  // Weakly informative (change if necessary)
  sd_gamma_st ~ gamma(5, 20);
  sd_gamma_poll ~ gamma(5, 20);
  sd_phi_st ~ gamma(5, 20); // on exp scale!
  sd_phi_poll ~ gamma(5, 20);
}
