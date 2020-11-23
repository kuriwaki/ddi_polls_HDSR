library(tidyverse)
library(janitor)
library(fs)


# 2016 polls -----
e16 <- read_csv("http://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv")
path_2016 <- "data/input/surveys/538/president_general_polls_2016.csv"

if (!file_exists(path_2016))
  write_csv(e16, path_2016)


# 2016 ratings ----
ratings2016 <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/2016/pollster-ratings.csv") %>%
  rename_all(~str_replace(.x, "538", "fte")) %>%
  clean_names()

write_csv(ratings2016, "data/input/surveys/538/pollster_ratings_2016.csv")


# 2020 polls ----
polls_2020_raw <- read_csv("https://projects.fivethirtyeight.com/polls-page/president_polls.csv")
path_2020 <- "data/input/surveys/538/polls_2020.csv"
date_2020_modified <- as.Date(file_info(path_2020)$modification_time)

# remove some columns we'll never use
polls_2020 <- polls_2020_raw %>%
  select(-c(internal, notes, partisan, tracking, stage,
            seat_name, office_type, seat_number,
            ranked_choice_reallocated, nationwide_batch, election_date, sponsor_candidate))


# Update if the file is older than 2020
if (date_2020_modified <= "2020-10-21" | is.na(date_2020_modified))
  write_csv(polls_2020, path_2020)



# Pollster recode key ----
pollster_name_recode <- tribble(
  ~pollster_16, ~pollster_20, ~pollster_common,
  "Fox News/Anderson Robbins Research/Shaw & Company Research", "Fox News", "Fox News",
  "CNN/Opinion Research Corp.", "CNN/SSRS", "CNN",
  "Marquette University", "Marquette Law School", "Marquette University",
  "Selzer & Company", "Selzer & Co.", "Selzer",
  "Rasmussen Reports/Pulse Opinion Research", "Rasmussen Reports/Pulse Opinion Research", "Rasmussen",
  "TargetSmart/William & Mary", "TargetSmart", "TargetSmart",
  "Siena College", "Siena College/The New York Times Upshot", "Siena College",
  "Susquehanna Polling & Research, Inc.", "Susquehanna Polling & Research Inc.", "Susquehanna Polling & Research",
  "Mason-Dixon Polling & Research, Inc.", "Mason-Dixon Polling & Strategy", "Mason-Dixon Polling & Strategy",
  "Baldwin Wallace University", "Baldwin Wallace University (Great Lakes Poll)", "Baldwin Wallace University"
)

write_csv(pollster_name_recode, "data/input/surveys/538/pollster_name_recode.csv")
