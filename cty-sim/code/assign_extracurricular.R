rm(list = ls())

#### Assign Extracurricular places
library(raster)
library(data.table)
library(tidyverse)
library(Rcpp)

sourceCpp("code/action_by_gravity.cpp")

#### Load Data ----
wp <- fread("cty-sim/output/wp2.csv")
pers <- fread("cty-sim/output/pers_w_wid.csv")
hh <- fread("cty-sim/output/hh_coords.csv")
hf <- fread("cty-sim/output/hf.csv")
nh <- fread("cty-sim/output/nh.csv")
sch <- fread("cty-sim/output/sch.csv")

#### Assign "number of visitors" (as weight) to all WP, NH and HF
set.seed(4326 + 11)
wp_typew <- wp %>%
  filter(TYPE == "w")
wp_typew$VISITOR <- exp(rexp(nrow(wp_typew), 1.1)) - 1
summary(wp_typew$VISITOR)

wp_typen <- wp %>%
  filter(TYPE == "n") %>%
  left_join(nh %>% select(x, y, POP))
wp_typen$VISITOR <- wp_typen$POP / 2
wp_typen$VISITOR[is.na(wp_typen$VISITOR)] <- 0.001
summary(wp_typen$VISITOR)
wp_typen <- wp_typen %>% select(-POP)

wp_typehf <- wp %>%
  filter(TYPE == "hf")
wp_typehf$VISITOR <- wp_typehf$WORKER / 8 / 2
summary(wp_typehf$VISITOR)

loc_extracurr <- bind_rows(wp_typehf, wp_typen, wp_typew)
rm(wp_typehf, wp_typen, wp_typew)

#### Add coordinates to person file
pers <- pers %>%
  left_join(hh %>% select(HID, x, y))
pers <- pers %>%
  select(PID, HID, WID2, SID, NHID, x, y)
head(pers)

## Code Schools and NH with WID instead
wp_sch <- wp %>%
  filter(TYPE == "s") %>%
  arrange(SERIAL)

wp_nh <- wp %>%
  filter(TYPE == "n") %>%
  arrange(SERIAL)

tmp <- pers$SID[!is.na(pers$SID)]
pers$WID2[!is.na(pers$SID)] <- wp_sch$WID2[tmp]

tmp <- pers$NHID[!is.na(pers$NHID)]
pers$WID2[!is.na(pers$NHID)] <- wp_nh$WID2[tmp]

pers %>%
  filter(!is.na(WID2) & !is.na(SID) & !is.na(NHID))

#### Assign time, potentially VERY LONG ----
loc_mat <- loc_extracurr[,c("x", "y")] %>% as.matrix
weights <- loc_extracurr$VISITOR
weights <- ceiling(weights)
pts_mat <- pers[,c("x", "y")] %>% as.matrix

system.time(
  assign_mat <- assign_by_gravity2(pts = pts_mat, 
                                   locs = loc_mat, 
                                   weights = weights,
                                   num_loc_choose = 6, 
                                   num_loc_candidate = 1000, 
                                   seed = 4326 + 16, 
                                   steps = 1)
)

pid <- pers$PID[assign_mat[,1]]
wid_main <- pers$WID2[assign_mat[,1]]

ec_network_df <- data.frame(PID = pid,
                            WID_MAIN = wid_main)
ec_network_df[,paste0("WID_", 1:6)] <- NA
for (i in 1:6) {
  ec_network_df[,paste0("WID_", i)] <- loc_extracurr$WID2[assign_mat[,i+1]]
}


#### Manipulation of the extracurricular network
for (i in 1:5) {
  cond <- ec_network_df$WID_MAIN == ec_network_df[,paste0("WID_", i)]
  ind <- which(cond)
  ec_network_df[ind,paste0("WID_", i:5)] <- ec_network_df[ind,paste0("WID_", (i+1):6)]
}

## Check
tmp <- apply(ec_network_df[,2:7], 1, function (x) any(x[1] == x[2:6]))
summary(tmp)

## Drop sixth column
ec_network_df <- ec_network_df %>%
  select(-paste0("WID_", 6))
ec_network_df <- ec_network_df %>%
  select(-WID_MAIN)

ec_network_df <- ec_network_df %>%
  arrange(PID)

## Export
fwrite(ec_network_df, "cty-sim/output/extracurricular.csv")
