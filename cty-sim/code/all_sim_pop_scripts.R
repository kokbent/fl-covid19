args <- commandArgs(trailingOnly = T)

if(length(args) == 1) {
  outname <- tolower(args)
} else {
  stop("Illegal argument supplied.")
}

cat(paste0("Creating dataset with name of: ", outname, "\n"))

#### Build synthetic population sqlite using files in data folder

dir.create("cty-sim/output")
dir.create("cty-sim/tmp")

source("cty-sim/code/extract_ipums.R")
source("cty-sim/code/nh_dat_int.R")
source("cty-sim/code/allocate_hh.R")
source("cty-sim/code/sch_dat_int.R")
source("cty-sim/code/hf_dat_int.R")
source("cty-sim/code/wp_size_w_schnh.R")
source("cty-sim/code/assign_worker.R")
source("cty-sim/code/build_hh_network.R")
# source("cty-sim/code/build_neighbour_network.R")
# source("cty-sim/code/assign_extracurricular.R")
source("cty-sim/code/assign_comorbidity.R")
rm(list=ls())

args <- commandArgs(trailingOnly = T)
outname <- tolower(args)

cat(paste0("Creating dataset with name of: ", outname, "\n"))

source("cty-sim/code/export_gen_dat.R")
