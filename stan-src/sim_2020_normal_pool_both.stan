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
  vector<lower=0>[K] x_hat; // x_hat to regress on

  // Sigma Hierarchical (from MCMC)
  real phi_0;
  real<lower=0> sd_phi_0;
  vector[T_poll] phi_poll;
  vector<lower=0>[T_poll] sd_phi_poll;
  vector[N] phi_st;
  vector<lower=0>[N] sd_phi_st;

  // Mean Hierarchical (from MCMC)
  real gamma_0;
  real<lower=0> sd_gamma_0;
  vector[T_poll] gamma_poll;
  vector<lower=0>[T_poll] sd_gamma_poll;
  vector[N] gamma_st;
  vector<lower=0>[N] sd_gamma_st;

  // Additional Specification
  int<lower=0, upper=1> use_laplace;
}

parameters {
  // Lkhd
  vector<lower=-1, upper=1>[N] delta;
  real phi_0_est;
  vector[T_poll] phi_poll_est;
  vector[N] phi_st_est;
  real gamma_0_est;
  vector[T_poll] gamma_poll_est;
  vector[N] gamma_st_est;

  // Delta hierarchical
  real<lower=0> tau_delta;
  real mu_delta;
}

transformed parameters{
  matrix[N, T_poll] mu_reg_est;
  matrix<lower=0>[N, T_poll] sigma_reg_est;

  // Multilevel regression
  for (j in 1:T_poll) {
    for (i in 1:N) {
      mu_reg_est[i, j] = gamma_0_est + gamma_st_est[i] + gamma_poll_est[j];
      sigma_reg_est[i, j] = exp(phi_0_est + phi_st_est[i] + phi_poll_est[j]);
    }
  }
}

model {
  // Lkhd
  {
    vector[K] summands;
    if (use_laplace == 0) {
      for (k in 1:K) {
        summands[k] = normal_lpdf(error_vec[k] | delta[st[k]] + x_hat[k] * mu_reg_est[st[k], poll_type[k]],
          sigma_reg_est[st[k], poll_type[k]] * x_hat[k]);
      }
    } else {
      for (k in 1:K) {
        summands[k] = double_exponential_lpdf(error_vec[k] | delta[st[k]] + x_hat[k] * mu_reg_est[st[k], poll_type[k]],
          sigma_reg_est[st[k], poll_type[k]] * x_hat[k]);
      }
    }

    target += sum(summands);
  }

  // Prior specification
  phi_0_est ~ normal(phi_0, sd_phi_0);
  phi_poll_est ~ normal(phi_poll, sd_phi_poll);
  gamma_0_est ~ normal(gamma_0, sd_gamma_0);
  gamma_poll_est ~ normal(gamma_poll, sd_gamma_poll);
  phi_st_est ~ normal(phi_st, sd_phi_st);
  gamma_st_est ~ normal(gamma_st, sd_gamma_st);

  // Delta
  delta ~ normal(mu_delta, tau_delta / 10);
  tau_delta ~ lognormal(-1.2, 0.3);
  mu_delta ~ normal(0, 0.03);
}

generated quantities{
  vector[N] mu = delta + prev_results;
}
