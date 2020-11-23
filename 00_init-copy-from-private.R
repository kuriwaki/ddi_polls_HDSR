library(stringr)
library(fs)


pr_dir <- "~/Dropbox/ddi_polls"


dir_create("code")
file_copy(path = dir_ls(pr_dir,
                 regexp = "(00|01|03|04a|04b|05|44|5[0-3]|7[0-6])_"),
          new_path = "code",
          overwrite = TRUE)



source("01_initialize-dirs.R")


file_copy(path = path(pr_dir, "data/output/by-state_elecresults.rds"),
          new_path = "data/output/by-state_elecresults.rds",
          overwrite = TRUE)




# copy stan  fies
stan_dir <- path(pr_dir, "stan-src")
file_copy(path = dir_ls(stan_dir,
                        regexp = str_c(
                          "(",
                          str_c(
                            "robust_base1",
                            "fit_beta_poll_st",
                            "sim_2020_beta_both_pooled",
                            "fit_normal_pool_both",
                            "sim_2020_normal_pool_both",
                            collapse = "|"),
                          ")\\.stan")),
          new_path = "stan-src",
          overwrite = TRUE)
# stan output
