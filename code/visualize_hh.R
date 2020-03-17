rm(list = ls())

library(raster)
library(tidyverse)

source("code/data_path.R")

fl_cnt_shp <- shapefile(fl_cnt)
fl_hh_r <- raster(fl_hh)
hh_xy <- read_csv("output/hh_coords.csv")

#### Visualization
png("fig/hh_proposed.png", 2400, 2400, pointsize = 10, res = 300)
hh_sp <- SpatialPoints(hh_xy[,c("x", "y")], proj4string = crs(fl_hh_r))
plot(hh_sp, pch = ".", col = rgb(1, 0, 0, alpha = 0.2))
plot(fl_cnt_shp %>% spTransform(crs(fl_hh_r)), add = T)
dev.off()

tmp <- rasterize(hh_sp, fl_hh_r, fun = 'count')
png("fig/hh_density.png", 2400, 2400, pointsize = 10, res = 300)
plot(tmp)
dev.off()
