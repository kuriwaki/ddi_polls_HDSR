library(tidyverse)
library(kableExtra)
library(scales)
library(fs)

source("71_tabulate-counts_tab01.R")

# Data
polls_2016_only <- readRDS("data/output/538-polls_cleaned/data_2016_numbered.Rds")
wdc_2016 <- readRDS("data/output/538-polls_cleaned/data_2016_beta.Rds")

grade_2020 <- polls_2020 %>%
  distinct(fte_grade, coarse_grade, pollster)

polls_2016 <- polls_2016_only %>%
  left_join(select(wdc_2016, question_id, state, pollster, start_date, wdc_R_2pty = beta_w_R_2pty)) %>%
  mutate(pollster = pollster_fmt(pollster)) %>%
  left_join(grade_2020, by = "pollster")


# rho stats --
pp <- unit_format(accuracy = 0.1, scale = 100, unit = "pp")
sum_rho <- function(tbl) {
  tbl %>%
      summarize(
        mn_err = mean(est_R_2pty - rep_2pty),
        # sd_err = pp(sd(est_R_2pty - rep_2pty)),
        mn_rho = median(ddc_R_2pty/100),
        # sd_rho = sd(ddc_R_2pty/100),
        mn_wdc = mean(wdc_R_2pty/(10^4)),
        n  = as.character(n())) %>%
    arrange(mn_err) %>%
    mutate(mn_err = pp(mn_err))
}

actl <- polls_2016 %>% distinct(state, actl_R_2pty) %>%
  mutate(actl_R_2pty = paste0(format(round(actl_R_2pty, 1), nsmall = 1), "%"))

rho_states <- polls_2016 %>%
  group_by(state) %>%
  sum_rho() %>%
  left_join(actl, by = "state") %>%
  relocate(state, actl_R_2pty)


rho_all <- polls_2016 %>%
  sum_rho() %>%
  mutate(state = "Total")


rho_pollsters <- polls_2016 %>%
  group_by(pollster) %>%
  sum_rho()

options(scipen = 999)

bind_rows(rho_states, rho_all) %>%
  mutate(actl_R_2pty = replace_na(actl_R_2pty, "")) %>%
  kbl(format = "latex",
      format.args = list(big.mark = ",", digits = 1),
      col.names = c("State", "Actual", c("Error", "DDC*", "WDC"), "Polls"),
      linesep = "",
      align = c("l", rep("c", 1), rep("c", 1), "c"),
      booktabs = TRUE) %>%
  row_spec(row = 12, hline_after = TRUE) %>%
  write_lines("tables/HDSR_tab02_states.tex")

rho_pollsters %>%
  kbl(format = "latex",
      format.args = list(big.mark = ",", digits = 1),
      col.names = c("Pollster", c("Error", "DDC*", "WDC"), "Polls"),
      linesep = "",
      align = c("l", rep("c", 1), rep("c", 2), "c"),
      booktabs = TRUE) %>%
  write_lines("tables/HDSR_tab02_pollsters.tex")




anova_reg <- anova(lm(I(wdc_R_2pty) ~ state + pollster, polls_2016))
round(anova_reg$`Sum Sq` / sum(anova_reg$`Sum Sq`), 3)

anova(lm(I(wdc_R_2pty) ~ pollster, polls_2016))
anova(lm(I(wdc_R_2pty) ~ state, polls_2016))



