# args <- commandArgs(trailingOnly = T)
# 
# if(length(args) == 1) {
#   outname <- tolower(args)
# } else {
#   stop("Illegal argument supplied.")
# }
# 
# cat(paste0("Creating dataset with name of: ", outname, "\n"))

#### Build synthetic population sqlite using files in data folder

dir.create("pseudo-sim/output")
dir.create("pseudo-sim/tmp")

source("pseudo-sim/code/extract_ipums.R")
source("pseudo-sim/code/nh_dat_int.R")
source("pseudo-sim/code/allocate_hh.R")
source("pseudo-sim/code/sch_dat_int.R")
source("pseudo-sim/code/hf_dat_int.R")
source("pseudo-sim/code/wp_size_w_schnh.R")
source("pseudo-sim/code/assign_worker.R")
source("pseudo-sim/code/build_hh_network.R")
# source("pseudo-sim/code/build_neighbour_network.R")
# source("pseudo-sim/code/assign_extracurricular.R")
source("pseudo-sim/code/assign_comorbidity.R")
rm(list=ls())

# args <- commandArgs(trailingOnly = T)
outname <- "pseudo"

cat(paste0("Creating dataset with name of: ", outname, "\n"))

source("pseudo-sim/code/export_gen_dat.R")
