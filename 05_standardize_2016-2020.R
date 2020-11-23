library(tidyverse)
library(ddi)
library(fs)

source("44_useful_functions.R")

# Data ----
# created in 04a
data_2016 <- read_csv("data/output/538-polls_cleaned/538-polls_2016.csv")
# created in 04b
data_2020 <- read_rds("data/output/538-polls_cleaned/polls_2020_uq.Rds")
# previous results
results_df <- read_rds("data/output/by-state_elecresults.rds")

# Useful functions
source("44_useful_functions.R")

## Data Wrangling ----

# Define key states (no polls from Nevada, and seems to be a lot of hype around Texas)
key_st <- c(
  "Michigan", "New Hampshire", "Wisconsin", "Minnesota", "Pennsylvania", "Florida", "Arizona",
  "North Carolina", "Ohio", "Georgia", "Iowa", "Texas"
)

# Add necessary columns to data_2016
data_2016_cln <- data_2016 %>%
  filter(state %in% key_st) %>%
  mutate(
    est_R = est_R / 100,
    est_D = est_D / 100,
    rep_2pty = actl_R_2pty / 100,
    N_2pty = democrat + republican,
    n_2pty = ceiling(n * (est_R + est_D)),
    est_R_2pty = est_R / (est_R + est_D),
    ddc_R_2pty = ddc(rep_2pty, est_R_2pty, N = N_2pty, n = n_2pty) * 100
  )

# Join in previous results and compute useful summaries
data_2020_cln <- data_2016_cln %>%
  group_by(state) %>%
  summarise(
    totalvotes = unique(totalvotes),
    rep_2pty_prev = unique(rep_2pty),
    dem_2pty_prev = 1 - rep_2pty_prev,
    rep_totv_prev = unique(rep_totv),
    dem_totv_prev = unique(dem_totv)
  ) %>%
  left_join(data_2020, ., by = "state") %>%
  mutate(
    delta_rep_2pty_est = rep - rep_2pty_prev,
    o_hat_2pty = sqrt((totalvotes - n) / n) * 0.05
  ) %>%
  filter(state %in% key_st)


# Subset data
top_2016_pre <- data_2016_cln %>%
  get_top_pollsters(., ntop = 20) # seems reasonable

# ADD/REMOVE pollster
to_add <- c("Siena College", "Marquette University") # manually add some important ones (may be redundant)
top_2016 <- c(top_2016_pre, to_add)

to_remove <- c("SurveyMonkey")
top_2016 <- setdiff(top_2016, to_remove)

polls_sub <- data_2020_cln %>%
  filter(pollster %in% top_2016, state %in% unique(data_2016$state)) %>%
  mutate(
    state_num = as.numeric(as.factor(state)),
    pollster_num = as.numeric(as.factor(pollster)),
    f_inv_2pty = totalvotes / (n * 10^4) # doesn't include \sigma
  ) %>%
  # coarsen grade
  mutate(coarse_grade = str_extract(fte_grade, "[A-D]"),
         coarse_grade = recode(coarse_grade,
                               "B" = "B-C",
                               "C" = "B-C")) %>%
  relocate(question_id:state, pollster, pollster_formal, matches("grade"))


# Prepare previous results for joining in
results_to_join <- results_df %>%
  filter(year == 2012) %>%
  select(state, dem_2pty_prev = dem_2pty, rep_2pty_prev = rep_2pty)

# Data prep (filter, make sure state_num, pollster_nums are compatible) for st, pollster modelling
data_2016_sub <- data_2016_cln %>%
  filter(pollster %in% top_2016)

data_2016_numbered_pre <- polls_sub %>%
  group_by(pollster) %>%
  summarise(pollster_num = unique(pollster_num)) %>%
  left_join(data_2016_sub, ., by = "pollster") %>%
  filter(!is.na(pollster_num))

data_2016_numbered <- polls_sub %>%
  group_by(state) %>%
  summarise(state_num = unique(state_num)) %>%
  left_join(data_2016_numbered_pre, ., by = "state") %>%
  filter(!is.na(state_num)) %>%
  left_join(., results_to_join, by = "state") %>%
  mutate(
    delta_rep_2pty_est = est_R_2pty - rep_2pty_prev,
    o_hat_2pty = sqrt((N_2pty - n_2pty) / n_2pty) * 0.05
  )

# Add in Beta computation
data_2016_beta <- data_2016_numbered %>%
  mutate(
    f_inv_2pty = N_2pty / (n_2pty * 10^4),
    beta_w_R_2pty = (est_R_2pty - rep_2pty) / (f_inv_2pty * rep_2pty * (1 - rep_2pty))
  )


# 3 weeks subset
polls_three_wk <- polls_sub %>%
  filter(end_date >= "2020-10-01")


# Save analysis data -----

# delete old
if (file_exists("data/output/data_2016_numbered.Rds")) {
  file_delete("data/output/data_2016_numbered.Rds")
  file_delete("data/output/data_2020_sub.Rds")
}

# Save data -----
saveRDS(data_2016_numbered, "data/output/538-polls_cleaned/data_2016_numbered.Rds")
saveRDS(polls_sub, "data/output/538-polls_cleaned/data_2020_sub.Rds")
saveRDS(polls_three_wk, "data/output/538-polls_cleaned/data_2020_threeweek.Rds")
saveRDS(data_2016_beta, "data/output/538-polls_cleaned/data_2016_beta.Rds")
