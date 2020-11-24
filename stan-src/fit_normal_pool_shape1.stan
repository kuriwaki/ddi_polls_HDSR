data {
  // Initialization and data
  int<lower=0> K; // number of polls
  vector[K] reg_vec; // regressor (either beta or ddc)
  int<lower=1> T_poll; // number of poll types

  // Indicators
  int<lower=1, upper=T_poll> poll_type[K]; // indicator for type

  // Additional specification
  int<lower=0, upper=1> use_laplace;
}

parameters {
  // Sigma Hierarchical
  real phi_0;
  vector[T_poll] phi_poll;

  // Mean Hierarchical
  real gamma_0;
  vector[T_poll] gamma_poll;

  // Hyperparameters
  real<lower=0> sd_gamma_poll; // random effects on beta
  real<lower=0> sd_phi_poll;
}

transformed parameters {
  vector[T_poll] mu_reg_est = gamma_0 + gamma_poll;
  vector<lower=0>[T_poll] sigma_reg_est = exp(phi_0 + phi_poll);
}

model {
  // Lkhd
  if (use_laplace == 0) {
     reg_vec ~ normal(mu_reg_est[poll_type], sigma_reg_est[poll_type]);
  } else {
    reg_vec ~ double_exponential(mu_reg_est[poll_type], sigma_reg_est[poll_type]);
  }

  // Hierarchical
  phi_poll ~ normal(0, sd_phi_poll);
  gamma_poll ~ normal(0, sd_gamma_poll);

  // Weakly informative (change if necessary)
  sd_gamma_poll ~ gamma(5, 20);
  sd_phi_poll ~ gamma(5, 20);
}
