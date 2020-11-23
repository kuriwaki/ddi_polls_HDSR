data {
// Initialization and data
  int<lower=0> K; // number of polls
  vector<lower=-1, upper=1>[K] error_vec; // current poll results
  int<lower=0> N; // number of states
  vector<lower=0, upper=1>[N] prev_results; // results from previous year
  int<lower=0> T_poll; // number of poll types (not T so conflict with R)

  // Covariates and Indicators
  int<lower=1, upper=T_poll> poll_type[K]; // indicator for type
  int<lower=1, upper=N> st[K]; // indicator for state
  vector<lower=0>[K] o_hat; // o_hat to regress on

  // Bounds
  real<lower=0> rho_bound; // bound on |rho|
  vector[N] delta_L; // varying lower bound on delta
  vector[N] delta_U; // varying upper bound on delta

  // Priors
  real phi_0; // phi estimated from 2016
  real<lower=0> sd_phi_0;
  vector[N] phi_st;
  vector<lower=0>[N] sd_phi_st;
  vector[T_poll] phi_poll;
  vector<lower=0>[T_poll] sd_phi_poll;

  real beta_0; // beta estimates from 2016
  real<lower=0> sd_beta_0;
  vector[N] beta_st;
  vector<lower=0>[N] sd_beta_st;
  vector[T_poll] beta_poll;
  vector<lower=0>[T_poll] sd_beta_poll;
}

transformed data {
  vector[K] o_hat_scaled = o_hat / 10; // fix scaling issue
}

parameters {
  // Lkhd
  vector<lower=0, upper=1>[N] delta_raw;

  // Phi Hierarchical
  real phi_0_est;
  vector[N] phi_st_est;
  vector[T_poll] phi_poll_est;

  // Beta Hierarchical
  real beta_0_est;
  vector[N] beta_st_est;
  vector[T_poll] beta_poll_est;

  // Delta hierarchical
  real<lower=0> tau_delta;
  real mu_delta;
}

transformed parameters{
  matrix<lower=0, upper=1>[N, T_poll] mu_rho_est;
  matrix<lower=0>[N, T_poll] phi_rho_est;
  matrix<lower=0>[N, T_poll] rho_a;
  matrix<lower=0>[N, T_poll] rho_b;
  vector[N] delta  = delta_L + (delta_U - delta_L) .* delta_raw;

  // Multi-level regression
  for (j in 1:T_poll) {
    for (i in 1:N) {
      mu_rho_est[i, j] = inv_logit(beta_0_est + beta_st_est[i] + beta_poll_est[j]);
      phi_rho_est[i, j] = exp(phi_0_est + phi_st_est[i] + phi_poll_est[j]);
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
      summands[k] = beta_lpdf((error_vec[k] - delta[st[k]] + rho_bound * o_hat_scaled[k]) / (2 * o_hat_scaled[k] * rho_bound) |
        rho_a[st[k], poll_type[k]], rho_b[st[k], poll_type[k]]);
    }
    target += sum(summands);
  }

  // Hierarchical
  phi_st_est ~ normal(phi_st, sd_phi_st);
  phi_poll_est ~ normal(phi_poll, sd_phi_poll);
  beta_st_est ~ normal(beta_st, sd_beta_st);
  beta_poll_est ~ normal(beta_poll, sd_beta_poll);

  // Delta
  delta ~ normal(mu_delta, tau_delta / 10);
  tau_delta ~ lognormal(-1.2, 0.3);
  mu_delta ~ normal(0, 0.03);
}

generated quantities{
  vector[N] mu = delta + prev_results;
}
