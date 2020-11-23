library(tidyverse)
library(ggrepel)

source("70_vis-functions_hdsr.R")


model_dir <- "data/output/HDSR-models/"
fig3wk_df <- read_bind(c("posterior-mu_2020_baseline_3wk-ABC.Rds",
                          "posterior-mu_2020_rho-both_3wk-ABC.Rds",
                          "posterior-mu_2020_wdc-both_3wk-ABC.Rds")) %>%
  mutate(timeframe = "3wk")


gg_3wk <- hdsr_histogram(fig3wk_df, title_text  = "Only Using Polls 3 Weeks Out", show = c("bsl", "wdc"))

ggsave("figures/HDSR_fig01_3weeks.pdf", plot = gg_3wk, w = 9, h = 5.8)


summ_3wk <- fig3wk_df %>%
  group_by(state, method) %>%
  summarize(mean_3wk = mean(est),
            sd_3wk = sd(est),
            skew_3wk = e1071::skewness(est))

summ_3mo <- fig01_df %>%
  group_by(state, method) %>%
  summarize(mean_3mo = mean(est),
            sd_3mo = sd(est),
            skew_3mo = e1071::skewness(est))

summ_33 <- left_join(summ_3wk, summ_3mo) %>%
  filter(method %in% c("bsl", "wdc"))

gg_mean <- ggplot(summ_33, aes(x = mean_3mo, y = mean_3wk, color = method)) +
  geom_point() +
  geom_abline(linetype = "dashed") +
  scale_fill_manual(values = c("bsl" = '#67a9cf', "ddc" = '#fdae61', "wdc" = '#ef8a62'),
                    labels = c(bsl = 'Assuming No Bias',
                               `wdc` = "WDC",
                               `ddc` = "DDC"),
                    aesthetics = c("colour", "fill")) +
  theme_bw() +
  labs(x = "Polls 3 months out",
       y = "Polls 3 weeks out",
       title = "Mean",
       color = NULL) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom")

gg_sd <- gg_mean + aes(x = sd_3mo, y = sd_3wk) +
  labs(x = "Polls 3 months out",
       y = "Polls 3 weeks out",
       title = "Standard Deviation")

gg_sk <- gg_mean + aes(x = skew_3mo, y = skew_3wk) +
  labs(x = "Polls 3 months out",
       y = "Polls 3 weeks out",
       title = "Skewness") +
  guides(color = FALSE)

(gg_mean + guides(color = FALSE)) + gg_sd +
  plot_annotation(caption = "Each point represents a summary statistic (a  mean or a standard deviation) state-model combination.")
ggsave("figures/HDSR_fig02_3weeks3months.pdf", w = 6, h = 3.5)
