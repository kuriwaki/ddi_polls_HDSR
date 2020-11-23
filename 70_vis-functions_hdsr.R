library(scales)
library(lemon) # repeated labels in facets
library(ggrepel) # text annotation
library(ggtext)
library(patchwork)



# state ordering
key_st <- c(
  "New Hampshire",
  "Minnesota",
  "Michigan",
  "Pennsylvania",
  "Wisconsin",
  "Arizona",
  "Florida",
  "North Carolina",
  "Georgia",
  "Ohio",
  "Iowa",
  "Texas")
names(key_st) <- key_st

#' Read in long a estimate of draws
read_longer <- function(x, dir = model_dir) {
  mname <- str_remove(str_remove(x, "(_3wk-ABC|)\\.Rds"), "posterior-mu_2020_") %>%
    str_remove_all("_grade.*") %>%
    str_remove_all("_3wk-ABC")

  mname <- recode(mname, boot = "bsl", baseline = "bsl",
                  `wdc-both` = "wdc", `rho-both` = "ddc")

  obj <- read_rds(path(dir, x))
  tib <- as_tibble(obj)
  long <- pivot_longer(tib, everything(),
                       names_to = "state", values_to = "est")

  long %>%
    mutate(method = mname)
}

read_bind <- function(vec, state_order = key_st) {
  map_dfr(vec, read_longer) %>%
    mutate(state = factor(state, levels = state_order)) %>%
    mutate(method = factor(method, levels = c("bsl", "wdc", "ddc")))
}

col_key <- c("bsl" = '#67a9cf', "ddc" = '#fdae61', "wdc" = '#ef8a62')

# Main plotting function
hdsr_histogram <- function(
  data,
  title_text = NULL,
  facet_nrow = 3,
  shortlegend = FALSE,
  show = c("bsl", "ddc", "wdc"),
  colors = col_key) {

  data <- data %>%
    filter(method %in% show)
  col_vec <- colors[show]

  lbl_stat <- data %>%
    # for each state - method
    group_by(state, method) %>%
    summarize(
      lbl = pp(mean(est)),
      .groups = "drop") %>%
    mutate(method_col = col_vec[method]) %>%
    # then by state
    group_by(state) %>%
    summarize(lbl = str_c(glue("<span style='color:{method_col};'>{lbl}</span>"),
                          collapse = "<br>"),
              .groups = "drop")

  ggplot(data, aes(x = est,  y = stat(width*density))) +
    facet_rep_wrap(~ state, nrow = facet_nrow, repeat.tick.labels = "x") +
    geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    geom_histogram(aes(fill = method), alpha = 0.65, binwidth = 0.001,
                   position = position_identity()) +
    scale_y_continuous(expand = expansion(add = c(0, 0.005))) +
    scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0.45, 0.55, 0.05)) +
    coord_cartesian(xlim = c(0.42, 0.55), ylim = c(0, 0.15)) +
    theme_classic() +
    geom_richtext(data = lbl_stat, aes(label = lbl, x = 0.434, y = 0.10),
                  fill = NA, label.color = NA) +
    scale_fill_manual(values = colors,
                      labels = c(bsl = 'Assuming __No Bias__',
                                 `wdc` = ifelse(shortlegend, "Account for __WDC__",  "Account for __Weighting Deficiency Coefficient (WDC)__"),
                                 `ddc` = ifelse(shortlegend, "Account for __DDC*__", "Account for upper bound of __Data Defect Correlation (DDC)__")),
                      aesthetics = c("colour", "fill")) +
    labs(x = "Trump's Estimated 2020 Two-Party Voteshare against Biden",
         y = "Posterior Probability",
         title = title_text,
         fill = "Method for Aggregating 2020 Polls") +
    guides(color = FALSE) +
    theme(legend.position = "top",
          legend.text = element_markdown(),
          legend.title = element_text(size = 10),
          axis.text = element_text(color = "black"),
          strip.background = element_rect(color = "transparent", fill = "lightgray"),
          strip.text = element_text(size = 11),
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.x = element_text(color = "black", size = 12),
          axis.title.x = element_text(size = 14))
}

# write
wri <- function(x, file, round, dir = "numbers/") {

  format(round(x, digits = round), nsmall = round) %>%
    write_lines(path(dir, file))

}

pp <- function(x) format(round(100*x, digits = 1), nsmall = 1)
