data {
  // Initialization
  int<lower=0> K; // number of polls
  int<lower=0> N; // number of states

  // Covariates and Indicators
  int<lower=1, upper=N> st[K]; // indicator for state
  vector[K] n_vec; // sample size

  // Data
  real<lower=-1, upper=1> error_vec[K]; // error or proxy
  vector<lower=-1, upper=1>[N] prev_results; // previous results for generated quantities
}

parameters {
  //vector[N] delta_ast; // dummy for differences
  vector[N] delta;

  // Delta hyperparameters
  real<lower=0> tau_delta;
  real mu_delta;
}

model {
  // Hierarchical
  delta ~ normal(mu_delta, tau_delta / 10);

  // Weakly informative
  tau_delta ~ lognormal(-1.2, 0.3);
  mu_delta ~ normal(0, 0.03);

  // Likelihood
  error_vec ~ double_exponential(delta[st], 1 ./ (2 * sqrt(n_vec)));
}

generated quantities{
  vector[N] mu = delta + prev_results;
}
