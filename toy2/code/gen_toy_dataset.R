rm(list=ls())

library(tidyverse)

#### Generate Toy Data

#### IPUMS ----
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
source("code/data_path.R")

## Load original
ddi <- read_ipums_ddi(ipums_ddi)
data <- read_ipums_micro(ddi)

## Identify the lines that are within Escambia_02
ind <- which(data$PUMA %in% c(3301, 3302))

dat_in <- read_lines("data/usa_00002_full.dat")
dat_out <- dat_in[ind]

## Write data and zip
write_lines(dat_out, "toy2/data/usa_00002.dat")
tar("toy2/data/usa_00002.tar.gz", "toy2/data/usa_00002.dat")

## Test if it works
ddi <- read_ipums_ddi("toy2/data/usa_00002.xml")
data <- read_ipums_micro(ddi)


#### Census Tract Shape ----
library(raster)
library(rgdal)

if (dir.exists("./tmp/cenacs")) {
  cenacs <- shapefile("./tmp/cenacs/cenacs_2018.shp")
} else {
  untar(p2_cenacs, exdir = "./tmp")
  cenacs <- shapefile("./tmp/cenacs/cenacs_2018.shp")
}

ct_to_puma <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt")
ct_to_puma <- ct_to_puma %>%
  filter(STATEFP == "12")

cenacs@data <- left_join(cenacs@data,
                         ct_to_puma,
                         by = c("STATEFP10" = "STATEFP", "COUNTYFP10" = "COUNTYFP", "TRACTCE10" = "TRACTCE"))

cond <- cenacs$PUMA5CE %in% c("03301", "03302")
cenacs1 <- cenacs[cond,]
cenacs1@data <- cenacs1@data %>% select(-PUMA5CE)
writeOGR(cenacs1, dsn = "toy2/data", layer = "cenacs_2018", driver = "ESRI Shapefile", 
         overwrite_layer = T)

## Test if it works
cenacs <- shapefile("toy2/data/cenacs_2018.shp")

#### Population raster ----
fl_hh <- raster(p2_hh)

cenacs_wgs <- spTransform(cenacs, CRS("+init=epsg:4326"))
fl_hh <- crop(fl_hh, cenacs_wgs)

writeRaster(fl_hh, "toy2/data/flhh10.tif", overwrite=T)

## Test if it works
fl_hh <- raster("toy2/data/flhh10.tif")
plot(fl_hh)
plot(cenacs_wgs, add=T)

#### Nursing home ----
nh_raw <- read_csv(p2_nh)
nh_sp <- SpatialPoints(nh_raw[,c("X", "Y")], CRS("+init=epsg:4326"))
cond <- !is.na(over(nh_sp, cenacs_wgs)$STATEFP10)
nh <- nh_raw[cond,]

write_csv(nh, "toy2/data/FL_Nursing_Homes.csv")

## Test
nh <- read_csv("toy2/data/FL_Nursing_Homes.csv")
nh

#### Schools ----
if (dir.exists("./tmp/gc_schools/")) {
  gc_sch <- shapefile("./tmp/gc_schools/gc_schools_sep17.shp")
} else {
  untar(p2_gcsch, exdir = "./tmp")
  gc_sch <- shapefile("./tmp/gc_schools/gc_schools_sep17.shp")
}

gc_sch <- gc_sch[gc_sch$COUNTY == "ESCAMBIA",]
writeOGR(gc_sch, dsn = "toy2/data", layer = "gc_schools_sep17", 
         driver = "ESRI Shapefile", overwrite_layer = T)

## College and University category has additional file
cu2 <- read_csv("data/cu_wsize.csv") %>%
  filter(COUNTY == "ESCAMBIA")
write_csv(cu2, "toy2/data/cu_wsize.csv")


#### Workplace Area Characteristics ----
if (dir.exists("./tmp/lehdwac_blk_2015")) {
  wac <- shapefile("./tmp/lehdwac_blk_2015/lehdwac_blk_2015.shp")
} else {
  untar(p2_wac, exdir = "./tmp")
  wac <- shapefile("./tmp/lehdwac_blk_2015/lehdwac_blk_2015.shp")
}

wac <- wac[wac$COUNTYFP10 == "033",]
wac@data <- wac@data %>% dplyr::select(-SHAPE_AREA, -SHAPE_LEN)
writeOGR(wac, dsn = "toy2/data", layer = "lehdwac_blk_2015",
         driver = "ESRI Shapefile", overwrite_layer = T)

## Test if it works
wac <- shapefile("toy2/data/lehdwac_blk_2015.shp")

#### NCD Workplace data ----
if (file.exists("./data/geocoded_workplaces_w_naics_w_essential.csv")) {
  wp_coords <- read_csv("./data/geocoded_workplaces_w_naics_w_essential.csv")
} else { 
  # Need to fix these paths...
  untar(p2_ncdwp, exdir = "./tmp")
  wp_coords <- read_csv("tmp/geocoded_workplaces_w_naics.csv")
  write_csv(wp_coords, "./data/geocoded_workplaces_w_naics.csv")
  file.remove("tmp/geocoded_workplaces_w_naics.csv")
}

head(wp_coords)
wp_sp <- wp_coords[,c("x", "y")] %>%
  as.data.frame %>% as.matrix %>%
  SpatialPoints(proj4string = CRS("+init=epsg:4326")) %>%
  spTransform(crs(wac))
tmp <- over(wp_sp, wac)
cond <- !is.na(tmp$STATEFP10)
wp_coords <- wp_coords[cond,]
write_csv(wp_coords, "toy2/data/geocoded_workplaces_w_naics_w_essential.csv")

#### HF data ----
hf <- read_tsv("data/hf_ahca.tsv")
hf1 <- hf %>% filter(County == "Escambia")
write_csv(hf1, "toy2/data/hf_ahca.tsv")
