data {
  // Initialization and data
  int<lower=0> K; // number of polls
  int<lower=0> N; // number of states
  real<lower=0> rho_bound; // bound on scaled \rho
  vector<lower=-rho_bound, upper=rho_bound>[K] rho_vec; // vector of rho
  int<lower=1> T_poll; // number of poll types

  // Indicators
  int<lower=1, upper=N> st[K]; // indicator for state
  int<lower=1, upper=T_poll> poll_type[K]; // indicator for type
}

transformed data {
  vector<lower=0, upper=1>[K] std_rho_vec = (rho_vec + rho_bound) / (2 * rho_bound);
}

parameters {
  // Phi Hierarchical
  real phi_0;
  vector[N] phi_st;
  vector[T_poll] phi_poll;

  // Beta Hierarchical
  real beta_0;
  vector[N] beta_st;
  vector[T_poll] beta_poll;

  // Hyperparameters
  real<lower=0> sd_beta_st; // state-level random effect on state
  real<lower=0> sd_beta_poll;
  real<lower=0> sd_phi_st;
  real<lower=0> sd_phi_poll;
}

transformed parameters{
  matrix<lower=0, upper=1>[N, T_poll] mu_rho_est;
  matrix<lower=0>[N, T_poll] phi_rho_est;
  matrix<lower=0>[N, T_poll] rho_a;
  matrix<lower=0>[N, T_poll] rho_b;

  // Multi-level regression
  for (j in 1:T_poll) {
    for (i in 1:N) {
      mu_rho_est[i, j] = inv_logit(beta_0 + beta_st[i] + beta_poll[j]);
      phi_rho_est[i, j] = exp(phi_0 + phi_st[i] + phi_poll[j]);
    }
  }

  // Beta Params
  rho_a = mu_rho_est .* phi_rho_est;
  rho_b = (1 - mu_rho_est) .* phi_rho_est;
}

model {
  // Lkhd
  {
    vector[K] summands;
    for (k in 1:K) {
      summands[k] = beta_lpdf(std_rho_vec[k] | rho_a[st[k], poll_type[k]], rho_b[st[k], poll_type[k]]);
    }
    target += sum(summands);
  }

  // Hierarchical
  phi_st ~ normal(0, sd_phi_st);
  phi_poll ~ normal(0, sd_phi_poll);
  beta_st ~ normal(0, sd_beta_st);
  beta_poll ~ normal(0, sd_beta_poll);

  // Weakly informative (change if necessary)
  sd_beta_st ~ gamma(5, 20);
  sd_beta_poll ~ gamma(5, 20);
  sd_phi_st ~ gamma(5, 20); // on exp scale!
  sd_phi_poll ~ gamma(5, 20);
}
