library(fs)


pr_dir <- "~/Dropbox/ddi_polls"


file_copy(dir_ls(pr_dir,
                 regexp = "(00|01|03|04a|04b|05|44|5[0-3]|7[0-6])_"),
  new_path = ".", overwrite = TRUE)

source("01_initialize-dirs.R")
