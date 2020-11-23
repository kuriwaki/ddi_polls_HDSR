library(janitor)
library(tidyverse)
library(kableExtra)
library(fs)


wri_int <- function(x, file, dir = "numbers/") {

  format(as.integer(x), big.mark = ",") %>%
    write_lines(path(dir, file))

}

pollster_fmt <- function(x) {
  recode(x,
         `Siena College` = "New York Times / Siena",
         `Rasmussen` = "Rasmussen Reports",
         CNN = "CNN / SSRS")
}


# Data
polls_2016 <- readRDS("data/output/538-polls_cleaned/data_2016_numbered.Rds") %>%
  mutate(pollster = pollster_fmt(pollster))
polls_2020 <- readRDS("data/output/538-polls_cleaned/data_2020_sub.Rds") %>%
  mutate(pollster = pollster_fmt(pollster))

polls_3wks <- readRDS("data/output/538-polls_cleaned/data_2020_threeweek.Rds")

# Tabulate
t16_total <- polls_2016 %>%
  summarize(n16 = n(),
            p16 = n_distinct(pollster)) %>%
  mutate(state = "Total")

t20_total <- polls_2020 %>%
  summarize(n20 = n(),
            p20 = n_distinct(pollster)) %>%
  mutate(state = "Total")


t16 <- polls_2016 %>%
  group_by(state) %>%
  summarize(n16 = n(),
            p16 = n_distinct(pollster)) %>%
  bind_rows(t16_total)

t20 <- polls_2020 %>%
  group_by(state) %>%
  summarize(n20 = n(),
            p20 = n_distinct(pollster)) %>%
  arrange(desc(n20)) %>%
  bind_rows(t20_total)

p16 <- polls_2016 %>%
  group_by(pollster) %>%
  summarize(n16 = n(),
            s16 = n_distinct(state))

p20 <- polls_2020 %>%
  group_by(pollster) %>%
  summarize(n20 = n(),
            s20 = n_distinct(state)) %>%
  arrange(desc(n20))


# Date range (needs manual)
date_range_16 <- str_c(range(polls_2016$end_date), collapse = " - ")
date_range_20 <- str_c(range(polls_2020$end_date, na.rm = TRUE), collapse = " - ")

left_join(t20, t16, by = "state") %>%
  kbl(format = "latex",
      format.args = list(big.mark = ","),
      col.names = c("State", rep(c("Polls", "Pollsters"), 2)),
      linesep = "",
      booktabs = TRUE) %>%
  row_spec(row = 12, hline_after = TRUE) %>%
  add_header_above(c(" " = 1, "2020" = 2, "2016" = 2)) %>%
  write_lines("tables/HDSR_tab01_states.tex")


left_join(p20, p16, by = "pollster") %>%
  kbl(format = "latex",
      format.args = list(big.mark = ","),
      col.names = c("Pollster", rep(c("Polls", "States"), 2)),
      linesep = "",
      booktabs = TRUE) %>%
  add_header_above(c(" " = 1, "2020" = 2, "2016" = 2)) %>%
  write_lines("tables/HDSR_tab01_pollsters.tex")



dropbox_path <- "numbers"
nrow(polls_2016) %>% wri_int("n_polls_2016.tex")
nrow(polls_2020) %>% wri_int("n_polls_2020.tex")
n_distinct(polls_2016$pollster) %>% wri_int("n_pollsters.tex")

state_n <- count(polls_2020, state, wt = n, sort = TRUE)
state_n$n[state_n$state == "Michigan"] %>% wri_int("n_total_michigan.tex")
state_n$n[state_n$state == "Iowa"] %>% wri_int("n_total_iowa.tex")
sum(polls_2020$n) %>% wri_int("n_respondents_2020.tex")


# 3 weeks
nrow(polls_3wks) %>% wri_int("n_polls_3wks.tex")
n_distinct(polls_3wks$pollster) %>% wri_int("n_pollsters_3wks.tex")


# pollster grade and method

polls_2020 %>%
  count(pollster, fte_grade, sort = TRUE) %>%
  kbl(format = "latex",
      align = "lcc",
      format.args = list(big.mark = ","),
      col.names = c("Pollster", "538 Grade", "Polls"),
      linesep = "",
      booktabs = TRUE) %>%
  write_lines("tables/HDSR_tab03_pollster-grades.tex")

