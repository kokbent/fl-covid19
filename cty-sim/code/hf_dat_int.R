rm(list = ls())

#### Integrate School data into the population
library(raster)
library(data.table)
library(tidyverse)
library(Rcpp)

source("cty-sim/code/data_path.R")
sourceCpp("code/action_by_gravity.cpp")

#### Data import ----
## PERS
gen_pers <- fread("cty-sim/output/person_details.csv")
gen_hh <- fread("cty-sim/output/hh_coords.csv")

## HF
hf_dat <- fread(p2_hf)
hf_dat <- hf_dat %>%
  select(X, Y, AHCA_number, Name, Licensed_Beds) %>%
  filter(!is.na(X))

hf_sp <- hf_dat
coordinates(hf_sp) <- ~ X + Y
proj4string(hf_sp) <- CRS("+init=epsg:4326")

#### Assign households to HF ----
## HF coordinates
hf_sp$HFID <- 1:nrow(hf_sp)
hf_coords <- coordinates(hf_sp)

## HH coordinates
hh_coords <- gen_hh[,c("x", "y")] %>%
  as.matrix

assign_mat <- assign_by_gravity(pts = hh_coords,
                                locs = hf_coords, 
                                weights = hf_sp$Licensed_Beds, 
                                num_loc = 3, 
                                seed = 4326 + 8)

gen_hh$HFID <- NA
gen_hh$HFID[assign_mat[,1]] <- assign_mat[,2]

#### Assign workers to HF (8 workers per 1 licensed bed) ----
hf_sp$WORKER <- hf_sp$Licensed_Beds * 8
hf_dat <- cbind(coordinates(hf_sp), hf_sp@data)

#### Export ----
fwrite(hf_dat, "cty-sim/output/hf.csv")
fwrite(gen_hh, "cty-sim/output/hh_coords.csv")
# fwrite(gen_pers, "output/person_details.csv")
