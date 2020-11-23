# Run scripts
set -o history -o histexpand
# Rscript 01_initilize_dirs.R || exit 1
Rscript 03_download-538.R || exit 1

Rscript 04a_standardize-538-2016-polls.R || exit 1
Rscript 04b_standardize-538-2020-polls.R || exit 1
Rscript 05_standardize_2016-2020.R || exit 1

# Rscript 08_elec_results.R || exit 1
# Rscript 21_clean-for-exploration.R || exit 1
# Rscript 50_sim_2020_prep.R || exit 1
# echo "finished ["!:0"] on script ["!:*"]"

Rscript 51_sim_2020_base.R || exit 1
echo "finished ["!:0"] on script ["!:*"]"

Rscript 52_sim_2020_ddc.R || exit 1
echo "finished ["!:0"] on script ["!:*"]"

Rscript 53_sim_2020_wdc.R || exit 1
echo "finished ["!:0"] on script ["!:*"]"


# Generate tables and counts (do figures by hand)
Rscript 71_tabulate-counts_tab01.R || exit 1
echo "finished ["!:0"] on script ["!:*"]"

Rscript 72_tabulate_rho-2016_tab02.R || exit 1
echo "finished ["!:0"] on script ["!:*"]"


