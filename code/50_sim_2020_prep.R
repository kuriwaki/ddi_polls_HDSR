library(tidyverse)

# Data -------
polls_sub <- readRDS("data/output/538-polls_cleaned/data_2020_sub.Rds") %>%
  mutate(f_inv_2pty = 1.1*f_inv_2pty,
         o_hat_2pty = 1.1*o_hat_2pty)

polls_three_wk <- readRDS("data/output/538-polls_cleaned/data_2020_threeweek.Rds")
data_2016_beta <- readRDS("data/output/538-polls_cleaned/data_2016_beta.Rds")

# Useful functions
source("44_useful_functions.R")

# Parameters and Variables
RHO_BOUND <- 1.1 # this can be made smaller (e.g. 0.6)
SEED <- 2020
NGRADES <- 2
DIR <- "data/output/HDSR-models"

# Define key states
key_st <- c(
  "Michigan", "New Hampshire", "Wisconsin", "Minnesota", "Pennsylvania", "Florida", "Arizona",
  "North Carolina", "Ohio", "Georgia", "Iowa", "Texas"
)

# Get reference df for i, and previous ordered results
state_ordered <- get_prev_res(polls_sub)
list2env(state_ordered, .GlobalEnv)

# Aggregate data for model
pollster_data_lst <- get_mod_data(polls_sub, data_2016_beta, "pollster", "pollster_num")
list2env(pollster_data_lst, .GlobalEnv)

## Subsetting by grade ----

# Split 2020 by grade and renumber (order A, B-C, D)
grade_order <- c("A", "BC")

grade_split <- split(polls_sub, polls_sub$coarse_grade) %>%
  map(~.x %>%
        mutate(pollster_num = as.numeric(as.factor(pollster))) # didn't refactor states b/c no subset
     )

names(grade_split) <- grade_order

# Split 2016 by above
graded_2016 <- vector(mode = "list", length = NGRADES)
names(graded_2016) <- grade_order
i <- 0

for (grade_df in grade_split) {
  i <- i + 1
  grade_pollsters <- unique(grade_df$pollster)
  graded_split_2016 <- data_2016_beta %>%
    filter(pollster %in% grade_pollsters)

  renumbered_graded <- grade_df %>%
    group_by(pollster) %>%
    summarise(pollster_num = unique(pollster_num)) %>%
    left_join(select(graded_split_2016, -pollster_num), ., by = "pollster")

  # Get bounds (to global env)
  bound_data <- get_mod_data(grade_df, renumbered_graded, "pollster", "pollster_num")
  names(bound_data) <- paste0(names(bound_data), sprintf("_grade%s", grade_order[i]))
  list2env(bound_data, .GlobalEnv)

  graded_2016[[i]] <- renumbered_graded
}


# Jointly renumber -----
# Quick joint renumbering function
renumber_joint <- function(current_df, hist_df, BOUND_SUFFIX, name_vec) {
  # Joint Renumbering
  current_df <- current_df %>%
    mutate(pollster_num = as.numeric(as.factor(pollster)))

  hist_df <- current_df %>%
    distinct(pollster, pollster_num) %>%
    inner_join(select(hist_df, -pollster_num), ., by = "pollster")

  # Get bounds
  bound_data <- get_mod_data(current_df, hist_df, "pollster", "pollster_num")
  names(bound_data) <- paste0(names(bound_data), BOUND_SUFFIX)
  list2env(bound_data, .GlobalEnv)

  # Output df
  df_lst <- list(current_df = current_df, hist_df = hist_df)
  names(df_lst) <- name_vec
  list2env(df_lst, .GlobalEnv)
}

# Release final data objects
renumber_joint(current_df = polls_sub,
               hist_df = data_2016_beta,
               BOUND_SUFFIX = "_gradeABC",
               name_vec = c("polls_wo_sm", "data_2016_abc"))

## Subsetting Time (note: still select same pollsters) ----

renumber_joint(polls_three_wk,
               data_2016_beta,
               BOUND_SUFFIX = "_three_wk",
               name_vec = c("polls_three_wk", "polls_2016_three_wk"))
