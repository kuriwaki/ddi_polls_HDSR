library(tidyverse)
library(ggrepel)

source("70_vis-functions_hdsr.R")

# Data ----
grd <- "_gradeABC"

fig01_df <- read_bind(
  c(glue("posterior-mu_2020_baseline{grd}.Rds"),
    glue("posterior-mu_2020_wdc-both{grd}.Rds"),
    glue("posterior-mu_2020_rho-both{grd}.Rds")))


# Stats by state -----
polls_2016 <- readRDS("data/output/538-polls_cleaned/data_2016_numbered.Rds")
beta_2016 <- readRDS("data/output/538-polls_cleaned/data_2016_beta.Rds")
polls_2020 <- readRDS("data/output/538-polls_cleaned/data_2020_sub.Rds")


p20 <- polls_2020 %>%
  group_by(state) %>%
  summarize(n_polls_2020 = n(),
            n_resp_2020 = sum(n))

p16 <- polls_2016 %>%
  group_by(state) %>%
  summarize(mean_ddc = mean(ddc_R_2pty),
            sd_ddc = sd(ddc_R_2pty),
            totalvotes = unique(totalvotes)) %>%
  left_join(group_by(beta_2016, state) %>% summarize(mean_wdc = mean(beta_w_R_2pty/1000)))

sum_state <- fig01_df %>%
  group_by(method, state) %>%
  summarize(mean = mean(est),
            sd = sd(est),
            .groups = "drop") %>%
  pivot_wider(id_cols = c(state),
              names_from = method,
              values_from = c(mean, sd)) %>%
  janitor::clean_names()  %>%
  mutate(
    sm_ddc = (mean_ddc - mean_bsl),
    sm_wdc = (mean_wdc - mean_bsl),
    ss_ddc = sd_ddc / sd_bsl,
    ss_wdc = sd_wdc / sd_bsl
  ) %>%
  left_join(p16, by = "state", suffix = c("_model", "_2016")) %>%
  left_join(p20)


# Median state
med_val <- sum_state %>%
  select(-state) %>%
  summarize(across(everything(), median))


dropbox_path <- "numbers"
med_val$ss_wdc %>% wri("sd_increase_wdc.tex", 1)
med_val$ss_ddc %>%  wri("sd_increase_ddc.tex", 1)
(100*(med_val$sm_wdc)) %>% wri("mn_increase_wdc.tex", 1)
(100*(med_val$sm_ddc)) %>%  wri("mn_increase_ddc.tex", 2)


# Figure "2" ------------

sum_long <- sum_state %>%
  select(state, matches("ss_"), totalvotes) %>%
  pivot_longer(-c(state, totalvotes),
               names_pattern = "ss_(wdc|ddc)",
               names_to = "model",
               values_to = "ss") %>%
  mutate(model = factor(model, levels = c("wdc", "ddc")))

sum_long %>%
  filter(model %in% c("wdc")) %>%
ggplot(aes(x = totalvotes, y = ss, color = model)) +
  stat_smooth(geom = "line", se = FALSE, method = "lm", alpha = 0.5) +
  geom_point() +
  geom_text_repel(aes(label = state), alpha = 0.5, color = "black") +
  labs(x = "Population Size (2016 Votes Cast)",
       y = "Standard Deviation from WDC Model\nas a Ratio of Baseline Model") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_x_continuous(labels = comma) +
  coord_cartesian(ylim = c(0.3, 3.5)) +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_manual(values = col_key) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_markdown(size = 12),
        axis.title = element_text(size = 12.5))

ggsave("figures/HDSR_fig03_spread-by-N.pdf",
       w = 5, h = 4)
