library(tidyverse)
library(glue)
library(fs)

source("70_vis-functions_hdsr.R")

# Download  data ----
model_dir <- "data/output/HDSR-models/"
grd <- "_gradeABC"

fig01_df <- read_bind(
  c(glue("posterior-mu_2020_baseline{grd}.Rds"),
    glue("posterior-mu_2020_wdc-both{grd}.Rds"),
    glue("posterior-mu_2020_rho-both{grd}.Rds")))



# Figure 1 setup -----
fig01_ann <- tribble(
  ~state, ~est, ~ypos, ~txt, ~method,
  "North Carolina", 0.440, 0.035,"Trump\nmore likely\nto lose", "bsl",
  "North Carolina", 0.535, 0.12, "Trump about\nequally likely\nto win", "wdc",
) %>%
  mutate(state = factor(state, levels = levels(fig01_df$state)))

fig01_arr <- tribble(
  ~state,      ~x1, ~x2, ~y1, ~y2, ~method,
  "North Carolina", 0.460, 0.485, 0.045, 0.07, "bsl",
  "North Carolina", 0.520, 0.505, 0.10, 0.05, "wdc"
)  %>%
  mutate(state = factor(state, levels = levels(fig01_df$state)))

# Figure 1 main ------
gg_fig01 <- hdsr_histogram(fig01_df, show = c("bsl", "wdc")) +
  geom_text(data = fig01_ann, aes(label = txt, y = ypos, color = method),
                  lineheight = 0.8, size = 2.5) +
  geom_segment(data = fig01_arr, aes(x = x1, xend = x2, y = y1, yend = y2, color = method))

ggsave("figures/HDSR_fig01.pdf", plot = gg_fig01, w = 9, h = 5.6)


# Figure 1 - stacked ver

grd <- "_gradeA"
fig01_dfA <- read_bind(
  c(glue("posterior-mu_2020_baseline{grd}.Rds"),
    glue("posterior-mu_2020_wdc-both{grd}.Rds"),
    glue("posterior-mu_2020_rho-both{grd}.Rds")))

grd <- "_gradeBC"
fig01_dfBC <- read_bind(
  c(glue("posterior-mu_2020_baseline{grd}.Rds"),
    glue("posterior-mu_2020_wdc-both{grd}.Rds"),
    glue("posterior-mu_2020_rho-both{grd}.Rds")))

gg_all <- hdsr_histogram(fig01_df, title_text = "All Pollsters", facet_nrow = 12, shortlegend = TRUE)
gg_A   <- hdsr_histogram(fig01_dfA, title_text = "Only Grade A Pollsters", facet_nrow = 12, shortlegend = TRUE)
gg_BC  <- hdsr_histogram(fig01_dfBC, title_text = "Only Grade B/C Pollsters", facet_nrow = 12, shortlegend = TRUE)

gg_bygrade <- (gg_all + labs(x = NULL) + guides(guide_legend(title.position = "top"))) +
  (gg_A  + guides(fill = FALSE, color = FALSE) + labs(x = "Trump's Estimated 2020\nTwo-Party Voteshare")) +
  (gg_BC + labs(x = NULL) + guides(fill = FALSE, color = FALSE)) +
  plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "top")
ggsave("figures/HDSR_fig01_bygrade.pdf", plot = gg_bygrade, w = 8, h = 17)


# gg_stacked <-  (gg_all + labs(x = NULL)) +
#   (gg_ABC + guides(fill = FALSE) + labs(x = NULL)) +
#   (gg_A + guides(fill = FALSE) + labs(x = NULL)) +
#   (gg_BC + guides(fill = FALSE) + labs(x = NULL)) +
#   (gg_D + guides(fill = FALSE)) +
#   plot_layout(ncol = 1)
# ggsave("figures/HDSR_fig01_both_allsubsets.pdf", plot = gg_stacked, w = 20, h = 11)
#
