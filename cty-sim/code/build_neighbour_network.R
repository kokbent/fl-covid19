rm(list = ls())
library(raster)
library(tidyverse)
library(abmgravity)
library(data.table)
library(future)
library(furrr)
source("cty-sim/code/data_path.R")

coeff <- 0.45
ncore <- 4 

#### Build household network
pers <- fread("cty-sim/output/pers_w_wid.csv")
hh <- fread("cty-sim/output/hh_coords.csv")
nh <- fread("cty-sim/output/nh.csv")

hh_nh <- inner_join(hh, nh %>% select(x, y))
hh_nonnh <- hh %>%
  filter(!HID %in% hh_nh$HID)

## Determine HH size
hh_count <- pers %>%
  group_by(HID) %>%
  count()

hh_nonnh <- hh_nonnh %>%
  left_join(hh_count)

sum(hh_nonnh$n)

## Determine local pop-den
hh_spdf <- hh_nonnh[,c("x", "y", "n")]
coordinates(hh_spdf) <- ~ x + y
proj4string(hh_spdf) <- CRS("+init=epsg:4326")

ras <- raster(p2_hh)
popden_ras <- rasterize(hh_spdf, ras, "n", fun = 'sum')
popden <- raster::extract(popden_ras, hh_spdf)
lpopden <- log10(popden)

## Determine the "capacity" of each household
## Rules: (1) Household mean = HH member size
set.seed(5354)
hh_nonnh$lambda <- with(hh_nonnh, lpopden * n * coeff)
hh_nonnh$neighbour <- rpois(nrow(hh_nonnh), hh_nonnh$lambda)
sum(hh_nonnh$neighbour == 0)
hh_wneighbour <- hh_nonnh %>%
  filter(neighbour != 0)

repl <- rep(1:nrow(hh_wneighbour), hh_wneighbour$neighbour)
pts_mat <- hh_wneighbour[repl, c("HID", "x", "y")] %>% as.matrix
rownames(pts_mat) <- NULL
loc_mat <- hh_wneighbour[, c("HID", "x", "y")] %>% as.matrix
weights <- hh_wneighbour$neighbour


slice_size <- floor(nrow(pts_mat) / ncore)
df <- data.frame(start = 0:(ncore - 1) * slice_size + 1,
                 end = 1:ncore * slice_size)
df$end[ncore] <- nrow(pts_mat)

rm(pers, hh, nh, hh_nonnh, hh_nh, hh_spdf, hh_wneighbour, ras, popden_ras)
print(df)

## Run away with it!
plan(multisession, workers = ncore)

assign_nb <- function (pts_mat, loc_mat, weights, i) {
  assign_mat <- assign_by_gravity(pts = pts_mat[,2:3],
                                  locs = loc_mat[,2:3],
                                  weights = weights,
                                  num_loc = 1000,
                                  seed = 4326 + i * 28,
                                  steps = 1)
  hid1 <- pts_mat[assign_mat[,1], 1]
  hid2 <- loc_mat[,1][assign_mat[,2]]
  
  hid_mat <- cbind(hid1, hid2)
  hid_mat
}

pts_mat_l <- list()

for (j in 1:ncore) {
  pts_mat_l[[j]] <- pts_mat[df$start[j]:df$end[j],]
}

rm(pts_mat)
gc()

system.time(
  edge_list <- future_map2(pts_mat_l, 1:ncore,
                           ~ assign_nb(.x, loc_mat, weights, .y))
)

edges <- do.call("rbind", edge_list)
edges <- as.data.table(edges)

plan(sequential)

## Export
fwrite(edges, "cty-sim/output/neighbour_network.csv")
