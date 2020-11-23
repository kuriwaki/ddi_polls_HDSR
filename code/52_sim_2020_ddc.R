library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
options(warn = 1)

# Useful functions
source("44_useful_functions.R")
source("50_sim_2020_prep.R") # contains all data loading etc.

# Models
fit_beta_both_mod <- stan_model("stan-src/fit_beta_poll_st.stan")
sim_2020_both_mod_ddc <- stan_model("stan-src/sim_2020_beta_both_pooled.stan")

# Saving
BASE_NAME <- sprintf("%s/posterior-mu_2020_rho-both", DIR)

## Functionized Run ----

# Fit Beta
run_fit_beta_ddc <- function(prev_df, nrun = 2000, chains = 4, step_size = 0.8) {

  # Data
  fit_beta_both_data <- list(
    K = nrow(prev_df),
    rho_bound = RHO_BOUND,
    rho_vec = prev_df$ddc_R_2pty,
    T_poll = length(unique(prev_df$pollster_num)),
    poll_type = prev_df$pollster_num,
    N = length(unique(prev_df$state_num)),
    st = prev_df$state_num
  )

  # Simulation
  beta_fit_both <- sampling(
    object = fit_beta_both_mod,
    data = fit_beta_both_data,
    iter = nrun,
    control = list(adapt_delta = step_size),
    seed = SEED,
    chains = chains
  )

  rstan::extract(beta_fit_both)
}

# Simulate 2020
run_sim_2020_ddc <- function(df, prev_res_ordered, ref_df, bound_df, beta_fit_both_extr,
                         fname, nrun = 5000, chains = 4, step_size = 0.8) {

  # Data + Priors
  sim_2020_both_data_pre <- list(
    K = nrow(df),
    error_vec = df$delta_rep_2pty_est,
    N = length(unique(df$state_num)),
    prev_results = prev_res_ordered,
    T_poll = length(unique(df$pollster_num)),
    poll_type = df$pollster_num,
    st = df$state_num,
    o_hat = df$o_hat_2pty,
    rho_bound = RHO_BOUND,
    delta_L = bound_df$lower_delta,
    delta_U = bound_df$upper_delta
  )

  prior_2016_data <- list(
    phi_0 = mean(beta_fit_both_extr$phi_0),
    sd_phi_0 = sd(beta_fit_both_extr$phi_0),
    phi_st = apply(beta_fit_both_extr$phi_st, 2, mean),
    sd_phi_st = apply(beta_fit_both_extr$phi_st, 2, sd),
    phi_poll = as.array(apply(beta_fit_both_extr$phi_poll, 2, mean)),
    sd_phi_poll = as.array(apply(beta_fit_both_extr$phi_poll, 2, sd)),
    beta_0 = mean(beta_fit_both_extr$beta_0),
    sd_beta_0 = sd(beta_fit_both_extr$beta_0),
    beta_st = apply(beta_fit_both_extr$beta_st, 2, mean),
    sd_beta_st = apply(beta_fit_both_extr$beta_st, 2, sd),
    beta_poll = as.array(apply(beta_fit_both_extr$beta_poll, 2, mean)),
    sd_beta_poll = as.array(apply(beta_fit_both_extr$beta_poll, 2, sd))
  )

  sim_2020_both_data <- c(sim_2020_both_data_pre, prior_2016_data)

  # Simulation
  sim_2020_both <- sampling(
    object = sim_2020_both_mod_ddc,
    data = sim_2020_both_data,
    iter = nrun,
    control = list(adapt_delta = step_size),
    seed = SEED,
    chains = chains
  )

  # Save results
  mu_sim_2020_both <- rstan::extract(sim_2020_both)$mu
  colnames(mu_sim_2020_both) <- ref_df$state

  saveRDS(mu_sim_2020_both, fname)
  sim_2020_both_data # for use in overdispersed
}

## Running Models

## All data ----

# Fit Beta
beta_fit_both_extr <- run_fit_beta_ddc(data_2016_beta)

# Simulate 2020
fname_base <- sprintf("%s.Rds", BASE_NAME)
sim_2020_both_data <- run_sim_2020_ddc(polls_sub, prev_res_ordered, ref_df, bound_df, beta_fit_both_extr, fname_base)

# Overdispersed modeling
vars_to_change <- c("sd_beta_0", "sd_beta_st", "sd_beta_poll")
run_overdispersed(sim_2020_both_mod_ddc, sim_2020_both_data, ref_df, DIR, "ddc", "", vars_to_change)

## 3 months ----

# Fit Beta
beta_fit_both_extr_abc <- run_fit_beta_ddc(data_2016_abc)

# Simulate 2020
fname_abc <- sprintf("%s_gradeABC.Rds", BASE_NAME)
sim_2020_both_data_abc <- run_sim_2020_ddc(
  polls_wo_sm, prev_res_ordered, ref_df,
  bound_df_gradeABC, beta_fit_both_extr_abc, fname_abc
)

## 3 weeks ----

# Fit Beta
beta_fit_both_extr_3wk <- run_fit_beta_ddc(polls_2016_three_wk)

# Simulate 2020
fname_3wk <- sprintf("%s_3wk-ABC.Rds", BASE_NAME)
sim_2020_both_data_3wk <- run_sim_2020_ddc(
  polls_three_wk, prev_res_ordered, ref_df,
  bound_df_three_wk, beta_fit_both_extr_3wk, fname_3wk
)

## By grade ----

for (i in 1:NGRADES) {
  print(i)

  # Fit Beta
  beta_fit_both_extr <- run_fit_beta_ddc(graded_2016[[i]])

  # Simulate 2020
  fname <- sprintf("%s_grade%s.Rds", BASE_NAME, grade_order[i])
  sim_2020_both_data <- run_sim_2020_ddc(
    grade_split[[i]], prev_res_ordered, ref_df, eval(sym(sprintf("bound_df_grade%s", grade_order[i]))),
    beta_fit_both_extr, fname, step_size = 0.9
  )
}
