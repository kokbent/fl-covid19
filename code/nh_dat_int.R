rm(list = ls())

# Nursing home and assisted living facilities
library(raster)
library(tidyverse)

source("code/data_path.R")

nh <- read_csv(fl_nh)
cenacs <- shapefile(fl_cenacs)

ct_to_puma <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt")
ct_to_puma <- ct_to_puma %>%
  filter(STATEFP == "12") # FL is 12

cenacs@data <- cenacs@data %>%
  left_join(ct_to_puma[,c("COUNTYFP", "TRACTCE", "PUMA5CE")], 
            by = c("COUNTYFP10" = "COUNTYFP" ,"TRACTCE10" = "TRACTCE"))

nh_sp <- SpatialPoints(nh[,c("X", "Y")], proj4string = CRS("+init=epsg:4326")) %>%
  spTransform(crs(cenacs))
nh_puma <- over(nh_sp, cenacs)

nh$PUMA5CE <- nh_puma$PUMA5CE
nh
nh$NHID <- 1:nrow(nh)
nh1 <- nh %>%
  select(NHID, X, Y, PUMA5CE, REP_POP = POPULATION)

write_csv(nh1, "output/nh.csv")
