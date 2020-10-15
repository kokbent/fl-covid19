rm(list = ls())

library(sf)
library(tidyverse)
library(data.table)

source("code/data_path.R")

fl_cnt_shp <- st_read(p2_cntx)
fl_cnt_shp <- st_transform(fl_cnt_shp, crs = 4326)
# fl_hh_r <- raster(fl_hh)
loc_xy <- fread("state-sim/sim_pop-florida/locations-florida.txt") 
hh_xy <- st_as_sf(loc_xy %>% filter(type == "h") %>%
                    select(locid, x, y), coords = c("x", "y"))
hh_xy <- st_set_crs(hh_xy, 4326)
hh_xy1 <- hh_xy %>% sample_frac(0.1)

wp <- st_as_sf(loc_xy %>% filter(type == "w") %>%
                 select(locid, x, y), coords = c("x", "y")) %>%
  st_set_crs(4326)
wp1 <- wp %>% sample_frac(0.1)

p1 <- ggplot() +
  geom_sf(data = fl_cnt_shp) +
  geom_sf(data = hh_xy1, pch = ".", colour = "#0000FF10") +
  geom_sf(data = wp1, pch = ".", colour = "#FF000010") +
  labs(title = "Household (blue) and Workplaces (red)") +
  theme_bw()
ggsave("hhwp.png", p1, dpi = 320, width = 20, height = 20, units = "cm")
