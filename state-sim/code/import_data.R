library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(stringr)
source("state-sim/code/full_data_path.R")

#### Unpacking data files, if needed ----
cat(paste0("Unpacking foundational datasets."))

if (file.exists(p2_cntx)) {
  cnt <- st_read(p2_cntx)
} else {
  untar(p2_cnt, exdir = "./tmp")
  cnt <- st_read("./tmp/cntdem/cntdem_acs_2018.shp")
}

cnt <- st_set_geometry(cnt, NULL)
fwrite(cnt %>% dplyr::select(TIGERNAME, FIPS), "./data/cnt.csv")

#### IPUMS ----
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

## Write data and zip
file.copy(ipums_dat, "./data/usa_00002.dat")
# tar("cty-sim/data/usa_00002.tar.gz", "cty-sim/data/usa_00002.dat")
file.copy(ipums_ddi, "./data/")

## Test if it works
ddi <- read_ipums_ddi("./data/usa_00002.xml")
data <- read_ipums_micro(ddi)


#### Census Tract Shape ----
if (file.exists(p2_cenacsx)) {
  cenacs <- st_read(p2_cenacsx)
} else {
  untar(p2_cenacs, exdir = "./tmp")
  cenacs <- st_read("./tmp/cenacs/cenacs_2018.shp")
}

cenacs <- cenacs %>% dplyr::select(-SHAPE_AREA, -SHAPE_LEN, -ALAND, -AWATER)
st_write(cenacs, dsn = "./data", layer = "cenacs_2018",
         driver = "ESRI Shapefile", delete_layer = TRUE,
         overwrite = T)

## Test if it works
cenacs <- shapefile("./data/cenacs_2018.shp")

#### Population raster ----
fl_hh <- raster(p2_hh)
writeRaster(fl_hh, "./data/flhh10.tif", overwrite=T)

#### Nursing home ----
nh_raw <- fread(p2_nh)
nh_sp <- SpatialPoints(nh_raw[,c("X", "Y")], CRS("+init=epsg:4326"))
cond <- !is.na(over(nh_sp, cenacs_wgs)$STATEFP10)
nh <- nh_raw[cond,]

write_csv(nh, "./data/FL_Nursing_Homes.csv")

## Test
nh <- fread("./data/FL_Nursing_Homes.csv")
nh

#### Schools ----
if (file.exists(p2_gcschx)) {
  gc_sch <- st_read(p2_gcschx)
} else {
  untar(p2_gcsch, exdir = "./tmp")
  gc_sch <- st_read("./tmp/gc_schools/gc_schools_sep17.shp")
}

st_write(gc_sch, dsn = "./data", layer = "gc_schools_sep17",
         driver = "ESRI Shapefile", delete_layer = TRUE,
         overwrite = T)


#### Workplace Area Characteristics ----
if (file.exists(p2_wacx)) {
  wac <- st_read(p2_wacx)
} else {
  untar(p2_wac, exdir = "./tmp")
  wac <- st_read("./tmp/lehdwac_blk_2015/lehdwac_blk_2015.shp")
}

wac <- wac %>% dplyr::select(-SHAPE_AREA, -SHAPE_LEN)
st_write(wac, dsn = "./data", layer = "lehdwac_blk_2015",
         driver = "ESRI Shapefile", delete_layer = TRUE,
         overwrite = T)


#### NCD Workplace data ----
if (file.exists(p2_ncdwpx)) {
  wp_coords <- fread(p2_ncdwpx)
} else {
  untar(p2_ncdwp, exdir = "./tmp")
  wp_coords <- fread("tmp/geocoded_workplaces_w_naics_w_essential.csv")
}

head(wp_coords)
wp_sp <- wp_coords[,c("x", "y")] %>%
  as.data.frame %>% as.matrix %>%
  SpatialPoints(proj4string = CRS("+init=epsg:4326"))
tmp <- over(wp_sp, cenacs_wgs)
cond <- !is.na(tmp$STATEFP10)
wp_coords <- wp_coords[cond,]
fwrite(wp_coords %>% dplyr::select(-X1), 
       "./data/geocoded_workplaces_w_naics_w_essential.csv")

#### HF data ----
hf <- fread(p2_hf)
fwrite(hf, "./data/hf_ahca.tsv", sep = "\t")

#### BRFSS ----
# brfss <- fread(p2_brfss)
# brfss <- proc_brfss(brfss)
# brfss$`_IMPCTY` <- str_pad(brfss$`_IMPCTY`,
#                            width = 3, side = "left", pad = "0")
# brfss <- brfss %>%
#   filter(`_IMPCTY` == fips)
# write_csv(brfss, "cty-sim/data/brfss_proc.csv")
