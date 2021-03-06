rm(list = ls())

#### Nursing home and assisted living facilities
library(raster)
library(tidyverse)

source("code/data_path.R")

#### Loading up data ----
if (dir.exists("./tmp/cenacs")) {
  cenacs <- shapefile("./tmp/cenacs/cenacs_2018.shp")
} else {
  untar(p2_cenacs, exdir = "./tmp")
  cenacs <- shapefile("./tmp/cenacs/cenacs_2018.shp")
}

nh <- read_csv(p2_nh)

ct_to_puma <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt")
ct_to_puma <- ct_to_puma %>%
  filter(STATEFP == "12") # FL is 12


#### Connect Census Tract with PUMA ----
cenacs@data <- cenacs@data %>%
  left_join(ct_to_puma[,c("COUNTYFP", "TRACTCE", "PUMA5CE")], 
            by = c("COUNTYFP10" = "COUNTYFP" ,"TRACTCE10" = "TRACTCE"))


#### Determine the PUMA of each NH ----
nh_sp <- SpatialPoints(nh[,c("X", "Y")], proj4string = CRS("+init=epsg:4326")) %>%
  spTransform(crs(cenacs))
nh_puma <- over(nh_sp, cenacs)

nh$PUMA5CE <- nh_puma$PUMA5CE


#### Tidy up, add ID, export
nh$NHID <- 1:nrow(nh)
nh1 <- nh %>%
  select(NHID, X, Y, PUMA5CE, REP_POP = POPULATION)

write_csv(nh1, "output/nh.csv")
