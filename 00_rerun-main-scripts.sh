# Run scripts
set -o history -o histexpand

# data
Rscript 03_download-538.R || exit 1
Rscript 04a_standardize-538-2016-polls.R || exit 1
Rscript 04b_standardize-538-2020-polls.R || exit 1
Rscript 05_standardize_2016-2020.R || exit 1

# Model
Rscript 51_sim_2020_base.R || exit 1
echo "finished ["!:0"] on script ["!:*"]"

Rscript 52_sim_2020_ddc.R || exit 1
echo "finished ["!:0"] on script ["!:*"]"

Rscript 53_sim_2020_wdc.R || exit 1
echo "finished ["!:0"] on script ["!:*"]"


# Tables and numebrs
Rscript 71_tabulate-counts_tab01.R || exit 1
Rscript 72_tabulate_rho-2016_tab02.R || exit 1

# Figures
Rscript 73_vis-hist_fig01.R || exit 1
Rscript 74_vis-timewindow_fig02.R || exit 1
Rscript 75_vis-by-N_fig03.R || exit 1
Rscript 76_vis-sens_fig04.R || exit 1

