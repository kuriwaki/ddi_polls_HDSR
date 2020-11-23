library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
options(warn = 1)

# Useful functions
source("44_useful_functions.R")
source("50_sim_2020_prep.R") # contains all data loading etc.

# For saving
BASE_NAME <- sprintf("%s/posterior-mu_2020_baseline", DIR)

# Model
base_mod <- stan_model("stan-src/robust_base1.stan")

## Robust Baseline Function ----

run_base <- function(df, prev_res_ordered, fname, nrun = 5000, chains = 4, step_size = 0.8) {
  base_data <- list(
    K = nrow(df),
    error_vec = df$delta_rep_2pty_est,
    N = length(unique(df$state_num)),
    prev_results = prev_res_ordered,
    st = df$state_num,
    n_vec = df$n
  )

  # Simulation
  base_2020 <- sampling(
    object = base_mod,
    data = base_data,
    iter = nrun,
    control = list(adapt_delta = step_size),
    seed = SEED,
    chains = chains
  )

  # Save
  base_mu <- rstan::extract(base_2020)$mu
  colnames(base_mu) <- ref_df$state

  saveRDS(base_mu, fname)
}

## Running Models ----

# All data
fname_base <- sprintf("%s.Rds", BASE_NAME)
run_base(polls_sub, prev_res_ordered, fname_base)

# By grade
i <- 0
for (grade_df in grade_split) {
  i <- i + 1
  fname_grade <- sprintf("%s_grade%s.Rds", BASE_NAME, grade_order[i])
  run_base(grade_df, prev_res_ordered, fname_grade)
}

# Al
fname_abc <- sprintf("%s_gradeABC.Rds", BASE_NAME)
run_base(polls_wo_sm, prev_res_ordered, fname_abc)

# Three week data in 2020
fname_3wk_base <- sprintf("%s_3wk-ABC.Rds", BASE_NAME)
run_base(polls_three_wk, prev_res_ordered, fname_3wk_base)
