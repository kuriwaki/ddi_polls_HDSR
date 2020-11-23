## Miscellaneous Short Functions ----
`%notin%` <- Negate(`%in%`)
inv_logit <- function(x) exp(x) / (1 + exp(x))
get_mean_mu <- function(x) apply(rstan::extract(x)$mu, 2, mean)
get_mean_sd <- function(x) apply(rstan::extract(x)$mu, 2, sd)

# Compute RMSE
calc_rmse <- function(y, mu) {
  sqrt(mean((y - mu)^2))
}

# Fit Beta to bounded variable via MOM
fit_beta <- function(vec, bound) {
  trans_vec <- (vec + bound) / (2 * bound)
  beta <- mean(trans_vec)
  list("beta" = mean(trans_vec), "phi" = beta * (1 - beta) / var(trans_vec) - 1)
}

## Compute coverage probability for Stan simulations ----
coverage_prob <- function(mu_sim, y_true, alpha = 0.05) {
  coverage_ind <- numeric(length(y_true))

  for (i in 1:length(y_true)) {
    p <- ecdf(mu_sim[, i])(y_true[i])
    if (min(p, 1 - p) > alpha / 2) {
      coverage_ind[i] <- 1
    }
  }
  mean(coverage_ind)
}

## Predictions and summary statistics for lmer models ----
delta_predict <- function(mod_lst) {
  j <- 0
  pred_df_lst <- vector(mode = "list", length = length(yr_vec))

  # Loop through and get df of parameters
  for (yr in yr_vec) {
    j <- j + 1

    mod <- mod_lst[[j]]
    pred_df <- as.data.frame(cbind(rownames(coef(mod)$st), coef(mod)$st[, 1]))
    colnames(pred_df) <-  c("st", "delta_pred")

    pred_df_lst[[j]] <- swing_results_st %>%
      filter(year == yr) %>%
      select(st, rep_2pty_prev) %>%
      left_join(pred_df, ., by = "st") %>%
      mutate_at(vars(-st), ~ as.numeric(as.character(.))) %>%
      mutate(
        year = yr,
        pred = rep_2pty_prev + delta_pred
      ) %>%
      select(year, st, pred)
  }

  pred_df_merged <- Reduce(rbind, pred_df_lst)

  # Compute RMSE
  rmse_yr <- pred_df_merged %>%
    left_join(., swing_results_st, by = c("st", "year")) %>%
    group_by(year) %>%
    summarise(
      rmse = calc_rmse(pred, rep_2pty),
      avg_error = mean(pred - rep_2pty)
    )

  list("pred_df_merged" = pred_df_merged, "rmse_yr" = rmse_yr)
}

## Loop through years and apply Stan model ----

# Entire dataset
stan_yr_loop <- function(model, data_format, df_lst, mu_true, st_lst, nrun = 2000, step_size = 0.8, n_chains = 4,
                         max_search = 10, inference_option = FALSE, test = FALSE) {
  # Storage
  stan_df_lst <- vector(mode = "list", length = length(yr_vec))
  summary_vec_lst <- vector(mode = "list", length = length(yr_vec))
  stan_mod_lst <- vector(mode = "list", length = length(yr_vec))

  # Loop through
  j <- 0
  for (yr in yr_vec) {
    print(j <- j + 1)

    # Setup Data
    df_yr <- df_lst[[j]]
    n_states <- length(mu_true[[j]])

    blr_data <- eval(data_format)

    # Run Model
    sim <- sampling(
      object = model,
      data = blr_data,
      iter = nrun,
      chains = n_chains,
      seed = 2020,
      control = list(adapt_delta = step_size, max_treedepth = max_search)
    )

    if (test == TRUE) {
      return(sim)
    }

    # Extract simulated
    if (inference_option == FALSE) {
      mu_sim <- rstan::extract(sim)$mu
    } else {
      mu_sim <- rstan::extract(sim)$mu[, 1, ]
    }

    # Various point estimates
    mu_est <- apply(mu_sim, 2, mean)
    mu_bottom_quantile <- round(apply(mu_sim, 2, function(x) quantile(x, 0.025)), 3)
    mu_top_quantile <- round(apply(mu_sim, 2, function(x) quantile(x, 0.975)), 3)

    # Tabulate
    out_df <- cbind.data.frame(rep(yr, n_states), st_lst[[j]], round(as.numeric(mu_est), 3), mu_bottom_quantile, mu_top_quantile)
    colnames(out_df) <- c("year", "st", "pred", "2.5%", "97.5%")

    stan_df_lst[[j]] <- out_df
    stan_mod_lst[[j]] <- sim

    # Coverage prob. and RMSE
    summary_vec_lst[[j]] <- c(
      yr,
      calc_rmse(mu_true[[j]], mu_est),
      coverage_prob(mu_sim, mu_true[[j]])
    )
  }

  # Summary dataframe
  stan_summary_df <- Reduce(rbind, summary_vec_lst) %>%
    as_tibble() %>%
    mutate_if(is.numeric, ~ round(., 3)) %>%
    as.data.frame()

  colnames(stan_summary_df) <- c("yr", "RMSE", "Coverage Prob.")
  rownames(stan_summary_df) <- NULL

  # Predictions dataframe
  stan_pred_df <- Reduce(rbind, stan_df_lst)

  list("summary" = stan_summary_df, "predictions" = stan_pred_df, "sims" = stan_mod_lst)
}

# Single Year
run_yr <- function(model, yr, df_lst, data_format, step_size = 0.8, max_search = 10, n_chains = 4, nrun = 2000) {
  j <- ((yr - 2000) / 4) + 1
  print(j)
  df_yr <- df_lst[[j]]
  n_states <- length(unique(df_yr$state_num))
  blr_data <- eval(data_format)

  # Run Model
  sampling(
    object = model,
    data = blr_data,
    iter = nrun,
    chains = n_chains,
    seed = 2020,
    control = list(adapt_delta = step_size, max_treedepth = max_search)
  )
}

## Functionize PPC for ddc_R_2pty ----
ppc_top_pollsters <- function(sim_extr, df_yr, lambda_fun, n_top, ppc_option = 0) {
  n_rep <- length(sim_extr$beta_0)

  # Top pollster calculation
  pollster_counts <- df_yr %>%
    group_by(pollster_num) %>%
    summarise(count = n(),
              pollster = unique(pollster)) %>%
    arrange(desc(count))

  top_pollster_df <- pollster_counts[1:n_top, ]

  top_pollster_num <- top_pollster_df$pollster_num
  top_pollster_name <- top_pollster_df$pollster
  top_pollster_counts <- top_pollster_df$count

  # Compute lambda for each num
  T_obs_df <- df_yr %>%
    filter(pollster %in% top_pollster_name) %>%
    group_by(pollster) %>%
    summarise(T_obs = lambda_fun(ddc_R_2pty))

  # Downsample and compute posterior replicates
  ppc_vec <- numeric(0)
  i <- 0

  for (j in top_pollster_num) {
    i <- i + 1
    obs_size <- top_pollster_counts[i]

    if (ppc_option == 0) {
      T_rep <- apply(sim_extr$sim_rho_pollster[, , j], 1, function(x) lambda_fun(sample(x, obs_size, replace = FALSE)))
    } else{
      T_rep <- apply(sim_extr$sim_rho_pollster[, 1, , j], 1, function(x) lambda_fun(sample(x, obs_size, replace = FALSE)))
    }
    ppc_vec <- c(ppc_vec, T_rep)
  }

  # Combine
  ppc_df <- cbind(
    rep(top_pollster_name, each = n_rep),
    as.data.frame(ppc_vec)
  )
  colnames(ppc_df) <- c("pollster", "T_rep")

  # Plotting
  ppc_df %>%
    ggplot(aes(x = T_rep)) + geom_histogram(aes(y = ..density..)) +
    geom_vline(data = T_obs_df, aes(xintercept = T_obs), color = "blue", linetype = "dashed") +
    facet_wrap(~ pollster)
}

## Get quick summary of Stan model via MLE ----
quick_summary <- function(model, data_format, df_lst, true_mu, algorithm = "LBFGS") {
  summary_vec <- numeric(5)
  j <- 0

  for (yr in yr_vec) {
    print(j <- j + 1)

    # Setup Data
    df_yr <- df_lst[[j]]
    n_states <- length(unique(df_yr$state_num))
    data <- eval(data_format)

    # Run optimization
    opt <- optimizing(model,
                      data = data,
                      as_vector = FALSE,
                      algorithm = algorithm,
                      seed = 2020)

    summary_vec[j] <- calc_rmse((opt$par)$mu, true_mu[[j]])
  }
  summary_vec
}

## Refactor pollsters/state and compute useful quantities ----
refactor_summarise <- function(df) {
  # Split by year and refactor
  proxy_split <- split(df, df$year)

  df_numeric_refactored <- proxy_split %>%
    map(~ .x %>%
          mutate(
            state_num = as.numeric(as.factor(state)),
            pollster_num = as.numeric(as.factor(pollster))
          )
    )

  # Compute number of polls per pollster
  n_polls_lst <- df_numeric_refactored %>%
    map(~ .x %>%
          group_by(pollster_num) %>%
          arrange(pollster_num) %>%
          summarise(poll_count = unique(poll_count)) %>%
          pull(poll_count)
    )

  # Calculate lower/upper bounds for delta
  lower_bound <- df_numeric_refactored %>%
    map(~ .x %>%
          group_by(state_num) %>%
          summarise(lower_delta = max(error_proxy_R_2pty_unscaled - RHO_BOUND * o_hat_2pty / 10)) %>%
          pull(lower_delta)
    )

  upper_bound <- df_numeric_refactored %>%
    map(~ .x %>%
          group_by(state_num) %>%
          summarise(upper_delta = min(error_proxy_R_2pty_unscaled + RHO_BOUND * o_hat_2pty / 10)) %>%
          pull(upper_delta)
    )

  # Compute true mu and state list (useful if compound)
  true_mu <- df_numeric_refactored %>%
    map(~ unique(pull(.x, rep_2pty)))

  st_lst <- df_numeric_refactored %>%
    map (~ unique(pull(.x, st)))

  # Max counts
  max_counts <- df_numeric_refactored %>%
    map(~ .x %>%
          group_by(pollster) %>%
          summarise(count = n()) %>%
          pull(count) %>%
          max
    )

  # Output
  list("df_numeric_refactored" = df_numeric_refactored, "n_polls_lst" = n_polls_lst,
       "lower_bound" = lower_bound, "upper_bound" = upper_bound, "max_counts" = max_counts,
       "true_mu" = true_mu, "st_lst" = st_lst)
}

## Make plot of top pollsters by group ----

# Get top pollsters
get_top_pollsters <- function(df, ntop = 2) {
  df %>%
    group_by(pollster) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice_head(n = ntop) %>%
    pull(pollster)
}

# Make plots
top_pollster_ddc_plot <- function(df, plot_var = "ddc_R_2pty", split_var, ntop = 2) {
  df_lst <- split(df, pull(df, split_var))
  plot_vec <- vector(mode = "list", length = length(df_lst))

  i <- 0
  for (df in df_lst) {
    print(i <- i + 1)
    top_pollsters <- get_top_pollsters(df, ntop)

    df_sub <- df %>%
      filter(pollster %in% top_pollsters)

    print(plot_vec[[i]] <- df_sub %>%
            ggplot(aes_string(x = plot_var)) + geom_histogram(aes(y = ..density..)) +
            facet_wrap(~ pollster))
  }
  plot_vec
}

## Skew Normal Generation (tune params) ----
skew_norm_lpdf <- function(x, alpha) dnorm(x, log = TRUE) + pnorm(alpha * x, log.p = TRUE)

samp_skew_normal <- function(xi = 0, omega = 1, alpha, n = 10^4, burn_in = 2000) {
  prop_vec <- rnorm(10 * n)
  exp_vec <- rexp(10 * n) # by representation

  i <- 0
  j <- 0
  skew_norm_samp <- numeric(n)

  while (i < n) {
    j <- j + 1
    if (exp_vec[j] > -skew_norm_lpdf(prop_vec[j], alpha) + log(2) + dnorm(prop_vec[j], log = TRUE)) {
      i <- i + 1
      skew_norm_samp[i] <- prop_vec[j]
    }
  }

  xi + omega * skew_norm_samp
}
## Bootstrap by State ----

srs_boot <- function(df, st, n_boot = 10^3) {
  st_2020 <- df %>%
    filter(state == !!st) %>%
    select(rep, n)

  boot_vec <- numeric(n_boot)

  for (i in 1:n_boot) {
    boot_vec[i] <- st_2020[sample.int(nrow(st_2020), replace = TRUE), ] %>%
      summarise(est_mu = weighted.mean(rep, n)) %>%
      as.numeric
  }

  boot_vec
}

custom_summary <- function(vec, st) {
  unname(c(st, mean(vec), sd(vec), quantile(vec, c(0.025, 0.25, 0.5, 0.75, 0.975))))
}

## Grouping function for Beta ----
get_mod_data <- function(current_df, hist_df, gp, gp_num, rho_bound = 1.1, beta_bound = 0.75, use_ddc = TRUE) {
  n_gp <- current_df %>%
    pull(!!gp) %>%
    unique %>%
    length

  # Fit Beta
  beta_vec <- numeric(n_gp)
  phi_vec <- numeric(n_gp)

  for (i in 1:n_gp) {
    # Subset
    gp_i <- current_df %>%
      filter(!!sym(gp_num) == i) %>%
      pull(!!gp) %>%
      unique()

    gp_historical <- hist_df %>%
      filter(!!sym(gp) == !!gp_i)

    # Fit and store
    beta_fit_lst <- fit_beta(gp_historical$ddc_R_2pty, rho_bound)

    beta_vec[i] <- beta_fit_lst$beta
    phi_vec[i] <- beta_fit_lst$phi
  }

  # Get induced bounds
  if (use_ddc == TRUE) {
    bound_df <- current_df %>%
      group_by(state) %>%
      summarise(
        lower_delta = max(delta_rep_2pty_est - rho_bound * o_hat_2pty / 10),
        upper_delta = min(delta_rep_2pty_est + rho_bound * o_hat_2pty / 10)
      )
  } else {
    bound_df <- current_df %>%
      group_by(state) %>%
      summarise(
        lower_delta = max(delta_rep_2pty_est - beta_bound * N_scaled / 4), # NOTE: may be outside \pm 1 (unpleasant but necessary)
        upper_delta = min(delta_rep_2pty_est + beta_bound * N_scaled / 4)
      )
  }

  list("phi_vec" = phi_vec, "beta_vec" = beta_vec, "bound_df" = bound_df)
}

## Get reference df by state and ordered previous results ----
get_prev_res <- function(df) {
  ref_df <- df %>%
    group_by(state_num) %>%
    summarise(state = unique(state))

  prev_res_ordered <- numeric(nrow(ref_df))

  i <- 0
  for (st in ref_df$state) {
    i <- i + 1
    prev_res_ordered[i] <- df %>%
      filter(state == !!st) %>%
      pull(rep_2pty_prev) %>%
      unique
  }

  list("ref_df" = ref_df, "prev_res_ordered" = prev_res_ordered)
}

## Run overdispersed (either DDC or WDC) ----
run_overdispersed <- function(mod, sim_2020_both_data, ref_df, DIR, MODTYPE, SUFFIX, vars_to_change,
                              lambda_vec = seq(1, 8, 0.5), nrun = 5000, chains = 4, step_size = 0.8) {

  # Setup
  sim_2020_inflated_data <- sim_2020_both_data

  trump_win_prob <- data.frame(matrix(NA, nrow = length(lambda_vec), ncol = length(key_st)), row.names = lambda_vec)
  posterior_means <-  data.frame(matrix(NA, nrow = length(lambda_vec), ncol = length(key_st)), row.names = lambda_vec)
  posterior_sd <- data.frame(matrix(NA, nrow = length(lambda_vec), ncol = length(key_st)), row.names = lambda_vec)

  colnames(trump_win_prob) <- ref_df$state
  colnames(posterior_means) <- ref_df$state
  colnames(posterior_sd) <- ref_df$state
  i <- 0

  for (lambda in lambda_vec) {
    print(i <- i + 1)

    # Data
    sim_2020_inflated_data[vars_to_change] <- lapply(
      sim_2020_both_data[vars_to_change],
      function(x) x * lambda
    )

    # Simulation
    sim_2020_inflated <- sampling(
      object = mod,
      data = sim_2020_inflated_data,
      iter = nrun,
      control = list(adapt_delta = step_size),
      seed = SEED,
      chains = chains
    )

    # Aggregate P(Trump wins)
    sim_extr_mu <- rstan::extract(sim_2020_inflated)$mu
    trump_win_prob[i, ] <- apply(sim_extr_mu, 2, function(x) 1 - ecdf(x)(0.5))
    posterior_means[i, ] <- apply(sim_extr_mu, 2, mean)
    posterior_sd[i, ] <- apply(sim_extr_mu, 2, sd)
  }

  # Save results
  saveRDS(trump_win_prob, sprintf("%s/trump-win-prob_2020_%s-overdispersed%s.Rds", DIR, MODTYPE, SUFFIX))
  saveRDS(posterior_means, sprintf("%s/posterior-mean_2020_%s-overdispersed%s.Rds", DIR, MODTYPE, SUFFIX))
  saveRDS(posterior_sd, sprintf("%s/posterior-sd_2020_%s-overdispersed%s.Rds", DIR, MODTYPE, SUFFIX))
}