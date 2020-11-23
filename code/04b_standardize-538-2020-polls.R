library(tidyverse)
library(fs)

# Load in data ----
polls_raw0 <- read_csv("data/input/surveys/538/polls_2020.csv")
names_recode <- read_csv("data/input/surveys/538/pollster_name_recode.csv")


# Main identifiers ------
polls_raw1 <- polls_raw0 %>%
  mutate(
    start_date = as.Date(start_date, "%m/%d/%y"),
    end_date = as.Date(end_date, "%m/%d/%y"),
    state = replace_na(state, "U.S.")
    ) %>%
  rename(pollster_formal = pollster,
         pollster = display_name) %>%
  relocate(poll_id, question_id, state, pollster, start_date, end_date)


# LIMIT/delete some samples by date and candidate------
polls_2020 <- polls_raw1 %>%
  # START FROM
  filter(end_date >= "2020-08-03") %>%
  group_by(poll_id, question_id) %>%
  filter(any(candidate_name %in% c("Joseph R. Biden Jr."))) %>% # Must include Biden (not Bernie, or generic Dem)
  ungroup()

## Data Wrangling ----
poll_data <- polls_2020  %>%
  select(
    question_id,
    poll_id,
    state,
    pollster,
    pollster_formal,
    fte_grade,
    n = sample_size,
    start_date,
    end_date,
    population
  ) %>%
  distinct()

# reshape to wide, to get Rep and Democrat side by side
poll_cand <- polls_2020 %>%
  filter(candidate_party %in% c("DEM", "REP")) %>%
  mutate(
    candidate_party = ifelse(candidate_party == "REP", "rep", "dem"),
    pct = pct / 100
  ) %>%
  select(poll_id, question_id, population, candidate_party, pct) %>%
  pivot_wider(names_from = candidate_party, values_from = pct) %>%
  mutate(
    rep = rep / (rep + dem),
    dem = 1 - rep
  )

# Merge -------
polls_joined <- inner_join(poll_data, poll_cand,
                        by = c("question_id", "poll_id", "population"))

# Recode for 2016 compatibility ----
polls_fmt1 <- polls_joined %>%
  left_join(names_recode, by = c("pollster" = "pollster_20")) %>%
  mutate(pollster = coalesce(pollster_common, pollster)) %>%
  select(-pollster_common,  -pollster_16)

# select to one estimate ----
polls_fmt <- polls_fmt1 %>%
  group_by(poll_id) %>%
  mutate(population = factor(population, levels = c("v", "rv", "lv", "a"))) %>%
  slice(1) %>% # For single poll, pick (1) v (2) rv  (3) lv (3) a
  ungroup()

# Save -----
if (file_exists("data/output/polls_2020_uq.Rds"))
  file_delete("data/output/polls_2020_uq.Rds")

# save to new place
write_rds(polls_fmt, "data/output/538-polls_cleaned/polls_2020_uq.Rds")
