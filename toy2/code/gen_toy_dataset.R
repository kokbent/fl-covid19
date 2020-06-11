rm(list=ls())

library(tidyverse)

#### Generate Toy Data

#### IPUMS ----
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(readr)
source("code/data_path.R")

## Load original
ddi <- read_ipums_ddi(ipums_ddi)
data <- read_ipums_micro(ddi)

## Identify the lines that are within Alachua
ind <- which(data$PUMA %in% c(101, 102))

dat_in <- read_lines("toy/data/usa_00002_full.dat")
dat_out <- dat_in[ind]

## Write data and zip
write_lines(dat_out, "toy/data/usa_00002.dat")
tar("toy/data/usa_00002.tar.gz", "toy/data/usa_00002.dat")

## Test if it works
ddi <- read_ipums_ddi("toy/data/usa_00002.xml")
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

table(cenacs$COUNTYFP10)
cenacs <- cenacs[cenacs$COUNTYFP10 == "001",]
writeOGR(cenacs, dsn = "toy/data", layer = "cenacs_2018", driver = "ESRI Shapefile")

## Test if it works
cenacs <- shapefile("toy/data/cenacs_2018.shp")

#### Population raster ----
fl_hh <- raster(p2_hh)

cenacs_wgs <- spTransform(cenacs, CRS("+init=epsg:4326"))
fl_hh <- crop(fl_hh, cenacs_wgs)

writeRaster(fl_hh, "toy/data/flhh10.tif")

## Test if it works
fl_hh <- raster("toy/data/flhh10.tif")
plot(fl_hh)

#### Nursing home ----
nh_raw <- read_csv(p2_nh)
nh <- nh_raw %>%
  filter(COUNTY == "ALACHUA")

write_csv(nh, "toy/data/FL_Nursing_Homes.csv")

## Test
nh <- read_csv("toy/data/FL_Nursing_Homes.csv")
nh

#### Schools ----
if (dir.exists("./tmp/gc_schools/")) {
  gc_sch <- shapefile("./tmp/gc_schools/gc_schools_sep17.shp")
} else {
  untar(p2_gcsch, exdir = "./tmp")
  gc_sch <- shapefile("./tmp/gc_schools/gc_schools_sep17.shp")
}

gc_sch <- gc_sch[gc_sch$COUNTY == "ALACHUA",]
writeOGR(gc_sch, dsn = "toy/data", layer = "gc_schools_sep17", 
         driver = "ESRI Shapefile")

## College and University category has additional file
cu <- read_csv("data/cu_wsize.csv") %>%
  filter(COUNTY == "ALACHUA")
write_csv(cu, "toy/data/cu_wsize.csv")


#### Workplace Area Characteristics ----
if (dir.exists("./tmp/lehdwac_blk_2015")) {
  wac <- shapefile("./tmp/lehdwac_blk_2015/lehdwac_blk_2015.shp")
} else {
  untar(p2_wac, exdir = "./tmp")
  wac <- shapefile("./tmp/lehdwac_blk_2015/lehdwac_blk_2015.shp")
}

wac <- wac[wac$COUNTYFP10 == "001",]
writeOGR(wac, dsn = "toy/data", layer = "lehdwac_blk_2015",
         driver = "ESRI Shapefile")

## Test if it works
wac <- shapefile("toy/data/lehdwac_blk_2015.shp")

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
write_csv(wp_coords, "toy/data/geocoded_workplaces_w_naics_w_essential.csv")
