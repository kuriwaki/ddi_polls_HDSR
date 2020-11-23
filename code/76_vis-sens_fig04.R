library(tidyverse)
library(patchwork)
library(scales)
library(ggrepel)
library(fs)

# Data ---
model_dir <- "data/output/HDSR-models/"
read_lambdas <- function(outcome, model, file, ver = "HDSR-models",  dir = "data/output") {
  read_rds(path(dir, ver, file)) %>%
    as_tibble(rownames = "lambda") %>%
    mutate(lambda = as.numeric(lambda),
           model = model) %>%
    pivot_longer(-c(lambda, model), names_to = "state", values_to = outcome)
}

abb_state <- function(x) {
  recode(x, Arizona = "AZ", `New Hampshire` = "NH", Wisconsin = "WI",
         Pennsylvania = "PA", Minnesota = "MN", Florida = "FL",
         `North Carolina` = "NC", Georgia = "GA", Michigan = "MI",
         Iowa = "IA", Ohio = "OH", Texas = "TX")

}


# Sensitivity to lambda ----
tr_lambda <- read_lambdas(outcome = "trump", "ddc", file = "trump-win-prob_2020_ddc-overdispersed.Rds") %>%
  arrange(desc(lambda), trump) %>%
  mutate(state = fct_inorder(state))

mu_lambda <- read_lambdas(outcome = "voteshare", "ddc", file = "posterior-mean_2020_ddc-overdispersed.Rds")
sd_lambda <- read_lambdas(outcome = "sd", "ddc",file = "posterior-sd_2020_ddc-overdispersed.Rds")

tr_wdc <- read_lambdas(outcome = "trump", "wdc",file = "trump-win-prob_2020_wdc-overdispersed.Rds") %>%
  arrange(desc(lambda), trump) %>%
  mutate(state = fct_inorder(state))

mu_wdc <- read_lambdas(outcome = "voteshare","wdc", file = "posterior-mean_2020_wdc-overdispersed.Rds")
sd_wdc <- read_lambdas(outcome = "sd", "wdc", file = "posterior-sd_2020_wdc-overdispersed.Rds")


params_lambda <- bind_rows(
  tr_lambda,
  mu_lambda,
  sd_lambda,
  tr_wdc,
  mu_wdc,
  sd_wdc
) %>%
  pivot_longer(-c(lambda, model, state), names_to = "param", values_drop_na = TRUE) %>%
  mutate(state = factor(state, levels = levels(tr_lambda$state)),
         param = factor(param, levels = c("trump", "voteshare", "sd")))


# Graph tips
st_ann <- params_lambda %>%
  filter(lambda == 8)

param_lbl <- c(trump = "Pr(Trump Two-Party Voteshare > 0.5)",
               voteshare = "Trump Two-Party Voteshare",
               sd = "Standard Deviation\naround Voteshare")

ylab_lbl <- c(trump = "Posterior Probability\nof Trump Victory",
              voteshare = "Posterior Mean",
              sd = "Posterior S.D.")

lean_D <- "#2166ac"
lean_R <- "#b2182b"
tossup <- "#1a9850"
colors_val <- c("Arizona" = lean_D,
                "New Hampshire" = lean_D,
                "Michigan" = lean_D,
                "Wisconsin" = lean_D,
                "Pennsylvania"  = lean_D,
                "Minnesota" = lean_D,
                "Florida" = tossup,
                "North Carolina" = tossup,
                "Georgia" = tossup,
                "Iowa" = lean_R,
                "Ohio" = lean_R,
                "Texas" = lean_R)


# Graph ---

graph_lambda <- function(p,
                         m,
                         data = params_lambda,
                         base_df = stats_bsl,
                         stat_df = st_ann,
                         colorcode = colors_val) {

  plot_df <- filter(data, param == p, model %in% m)
  stat_df <- filter(stat_df, param == p, model %in% m)

  # abbreviate state
  if (p %in% c("sd", "voteshare")) {
    stat_df <- stat_df %>%
      mutate(state = abb_state(state))
    colorcode_abb <- colorcode
    names(colorcode_abb) <- abb_state(names(colorcode))
    colorcode <- c(colorcode_abb, colorcode)
  }

  gg <- plot_df %>%
    ggplot(aes(x = lambda, y = value, group = str_c(state, "-", model), color = state,
               linetype = model)) +
    facet_wrap(~ param, labeller = labeller(param = param_lbl)) +
    geom_line(alpha = 0.75) +
    guides(color = FALSE, linetype = FALSE) +
    scale_x_continuous(expand = expansion(add = c(0.1, 2)),
                       breaks = seq(1, 9, 2)) +
    geom_text_repel(data = stat_df,
                    aes(label = state),
                    direction = "y",
                    segment.alpha = 0.1,
                    force = 3,
                    hjust = -0.5, size = 2.5) +
    theme_bw() +
    scale_color_manual(values = colorcode) +
    labs(y = ylab_lbl[p],
         x = "Uncertainty tuning parameter",
         linetype = "Model") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(color = "black"))

  if (p %in% c("trump")) {
    gg <- gg +
      scale_y_continuous(expand = expansion(add = c(0.01, 0.01)),
                         labels = percent_format(accuracy = 1))
  }

  if (p %in% "voteshare") {
    gg <- gg +
      geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
      scale_y_continuous(limits = c(0.3, 0.55),
                         expand = expansion(add = c(0.01, 0.01)),
                         labels = percent_format(accuracy = 1))
  }

  if (p %in% "sd") {
    gg <- gg +
      scale_y_continuous(expand = expansion(add = c(0, 0.005))) +
      expand_limits(y = 0)
  }
  return(gg)
}



gg_tr_d <- graph_lambda("trump", "ddc")
gg_vs_d <- graph_lambda("voteshare", "ddc")
gg_sd_d <- graph_lambda("sd", "ddc")

gg_tr_w <- graph_lambda("trump", "wdc")
gg_vs_w <- graph_lambda("voteshare", "wdc")
gg_sd_w <- graph_lambda("sd", "wdc")


gg_d <- gg_tr_d + (gg_vs_d + gg_sd_d) + plot_layout(nrow = 2)
gg_w <- gg_tr_w + (gg_vs_w + gg_sd_w) + plot_layout(nrow = 2)

ggsave("figures/HDSR_fig03_ddc.pdf", gg_d, w = 6, h = 5)
ggsave("figures/HDSR_fig03_wdc.pdf", gg_w, w = 6, h = 5)
