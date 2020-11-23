library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
options(warn = 1)

# Useful functions
source("44_useful_functions.R")
source("50_sim_2020_prep.R") # contains all data loading etc.

# Models
fit_normal_both_mod <- stan_model("stan-src/fit_normal_pool_both.stan")
sim_2020_both_mod_wdc <- stan_model("stan-src/sim_2020_normal_pool_both.stan")

# Saving
BASE_NAME <- sprintf("%s/posterior-mu_2020_wdc-both", DIR)

## Functionized Run ----

# Fit Normal
run_fit_normal <- function(prev_df, use_laplace = 0, nrun = 2000, chains = 4, step_size = 0.8) {
  fit_normal_both_data <- list(
    K = nrow(prev_df),
    reg_vec = prev_df$beta_w_R_2pty,
    T_poll = length(unique(prev_df$pollster_num)),
    poll_type = prev_df$pollster_num,
    N = length(unique(prev_df$state_num)),
    st = prev_df$state_num,
    use_laplace = use_laplace
  )

  # Simulation
  fit_normal_both <- sampling(
    object = fit_normal_both_mod,
    data = fit_normal_both_data,
    iter = nrun,
    control = list(adapt_delta = step_size),
    seed = SEED,
    chains = chains
  )

  rstan::extract(fit_normal_both)
}

# Simulate 2020
run_sim_2020_wdc <- function(df, prev_res_ordered, ref_df, fit_normal_both_extr,
                             fname, nrun = 5000, chains = 4, step_size = 0.8) {

  # Data
  sim_2020_both_data_pre <- list(
    K = nrow(df),
    error_vec = df$delta_rep_2pty_est,
    N = length(unique(df$state_num)),
    prev_results = prev_res_ordered,
    T_poll = length(unique(df$pollster_num)),
    poll_type = df$pollster_num,
    st = df$state_num,
    x_hat = df$f_inv_2pty / 4, # incorporate sigma
    use_laplace = 0
  )

  prior_2016_both_data <- list(
    phi_0 = mean(fit_normal_both_extr$phi_0),
    sd_phi_0 = sd(fit_normal_both_extr$phi_0),
    phi_st = apply(fit_normal_both_extr$phi_st, 2, mean),
    sd_phi_st = apply(fit_normal_both_extr$phi_st, 2, sd),
    phi_poll = as.array(apply(fit_normal_both_extr$phi_poll, 2, mean)),
    sd_phi_poll = as.array(apply(fit_normal_both_extr$phi_poll, 2, sd)),
    gamma_0 = mean(fit_normal_both_extr$gamma_0),
    sd_gamma_0 = sd(fit_normal_both_extr$gamma_0),
    gamma_st = apply(fit_normal_both_extr$gamma_st, 2, mean),
    sd_gamma_st = apply(fit_normal_both_extr$gamma_st, 2, sd),
    gamma_poll = as.array(apply(fit_normal_both_extr$gamma_poll, 2, mean)),
    sd_gamma_poll = as.array(apply(fit_normal_both_extr$gamma_poll, 2, sd))
  )

  sim_2020_both_data <- c(sim_2020_both_data_pre, prior_2016_both_data)

  # Simulation
  sim_2020_both <- sampling(
    object = sim_2020_both_mod_wdc,
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

# Fit Normal
normal_fit_both_extr <- run_fit_normal(data_2016_beta)

# Simulate 2020
fname_base <- sprintf("%s.Rds", BASE_NAME)
sim_2020_both_data <- run_sim_2020_wdc(polls_sub, prev_res_ordered, ref_df, normal_fit_both_extr, fname_base)

# Overdispersion Modeling
vars_to_change <- c("sd_gamma_0", "sd_gamma_st", "sd_gamma_poll")
run_overdispersed(sim_2020_both_mod_wdc, sim_2020_both_data, ref_df, DIR, "wdc", "", vars_to_change)

## 3 Months ----

# Fit Normal
normal_fit_both_extr_abc <- run_fit_normal(data_2016_abc)

# Simulate 2020
fname_abc <- sprintf("%s_gradeABC.Rds", BASE_NAME)
sim_2020_both_data_3wk <- run_sim_2020_wdc(
  polls_wo_sm, prev_res_ordered, ref_df, normal_fit_both_extr_abc, fname_abc
)

##  3-Week Subset ----

# Fit Normal
normal_fit_both_extr_3wk <- run_fit_normal(polls_2016_three_wk)

# Simulate 2020
fname_3wk_wdc <- sprintf("%s_3wk-ABC.Rds", BASE_NAME)
sim_2020_both_data_3wk <- run_sim_2020_wdc(
  polls_three_wk, prev_res_ordered, ref_df, normal_fit_both_extr_3wk, fname_3wk_wdc
)

## By grade ----

for (i in 1:NGRADES) {
  print(i)

  # Fit Normal
  normal_fit_both_extr <- run_fit_normal(graded_2016[[i]], nrun = 5000, step_size = 0.9)

  # Simulate 2020
  fname <- sprintf("%s_grade%s.Rds", BASE_NAME, grade_order[i])
  sim_2020_both_data <- run_sim_2020_wdc(
    grade_split[[i]], prev_res_ordered, ref_df,
    normal_fit_both_extr, fname, step_size = 0.9
  )
}
