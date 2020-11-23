library(tidyverse)


# 2016 polls (cleaned) ----
data_2016 <- read_csv("data/input/surveys/538/president_general_polls_2016.csv")
results_df <- read_rds("data/output/by-state_elecresults.rds")
names_recode <- read_csv("data/input/surveys/538/pollster_name_recode.csv")


# Modify -----
data_2016_all <- data_2016 %>%
  left_join(names_recode, by = c("pollster" = "pollster_16")) %>%
  mutate(pollster = coalesce(pollster_common, pollster)) %>%
  select(-pollster_common, -pollster_20) %>%
  filter(type == "polls-only", state != "District of Columbia") %>%
  rename(year = cycle) %>%
  select(-matches("mcmullin"), -matches("johnson"), -matches("adjpoll"),
         -c("branch", "type", "poll_id", "matchup", "forecastdate",
          "multiversions",  "poll_wt", "grade")) %>%
  left_join(results_df, by = c("year", "state")) %>%
  mutate(st = state.abb[match(state, state.name)],
         office = "Pres",
         dem_2pty = 100 * dem_2pty,
         rep_2pty = 100 * rep_2pty,
         elec_date = as.Date("2016-11-08"),
         startdate = as.Date(startdate, "%m/%d/%Y"),
         enddate = as.Date(enddate, "%m/%d/%Y")) %>%
  rename(n = samplesize,
         start_date = startdate,
         end_date = enddate,
         est_D = rawpoll_clinton,
         est_R = rawpoll_trump,
         actl_D_2pty = dem_2pty,
         actl_R_2pty = rep_2pty
  )

thresh_date <- as.Date("2016-08-08")

data_2016_cln <- data_2016_all %>%
  filter(state != "U.S.", end_date >= thresh_date) %>%
  drop_na(est_D, est_R)

# Write (as csv), so it can be tracked later if need be ----
write_csv(data_2016_cln, "data/output/538-polls_cleaned/538-polls_2016.csv", na = "")


