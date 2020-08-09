library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(stringr)
source("cty-sim/code/full_data_path.R")

#### Getting Rscript args, if any
args <- commandArgs(trailingOnly=TRUE)

#### Generate Toy Data using PUMA
if(length(args) < 1) {
  stop("No PUMA is supplied...")
} else {
  tar_puma <- args
}

# cat(paste0("Building foundation dataset for ", tar_cty, " county.\n"))

# if (file.exists(p2_cntx)) {
#   cnt <- st_read(p2_cntx)
# } else {
#   untar(p2_cnt, exdir = "./tmp")
#   cnt <- st_read("./tmp/cntdem/cntdem_acs_2018.shp")
# }
# 
# cnt <- st_set_geometry(cnt, NULL)
# fwrite(cnt %>% select(TIGERNAME, FIPS), "cty-sim/data/cnt.csv")
# fips <- as.character(cnt[cnt$TIGERNAME == tar_cty, "FIPS"])

#### IPUMS ----
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

## Load original
ddi <- read_ipums_ddi(ipums_ddi)
data <- read_ipums_micro(ddi)
data$PUMA1 <- str_pad(data$PUMA,
                      width = 5,
                      side = "left",
                      pad = "0")

## Identify the lines that are within target county
ind <- which(data$PUMA1 %in% tar_puma)

dat_in <- read_lines(ipums_dat)
dat_out <- dat_in[ind]

## Write data and zip
write_lines(dat_out, "cty-sim/data/usa_00002.dat")
tar("cty-sim/data/usa_00002.tar.gz", "cty-sim/data/usa_00002.dat")
file.copy(ipums_ddi, "cty-sim/data/")

## Test if it works
ddi <- read_ipums_ddi("cty-sim/data/usa_00002.xml")
data <- read_ipums_micro(ddi)


#### Census Tract Shape ----
if (file.exists(p2_cenacsx)) {
  cenacs <- st_read(p2_cenacsx)
} else {
  untar(p2_cenacs, exdir = "./tmp")
  cenacs <- st_read("./tmp/cenacs/cenacs_2018.shp")
}

ct_to_puma <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt")
ct_to_puma <- ct_to_puma %>%
  filter(STATEFP == "12")

cenacs <- left_join(cenacs,
                    ct_to_puma,
                    by = c("STATEFP10" = "STATEFP", "COUNTYFP10" = "COUNTYFP", "TRACTCE10" = "TRACTCE"))

cond <- cenacs$PUMA5CE %in% tar_puma
cenacs1 <- cenacs[cond,]
cenacs1 <- cenacs1 %>% select(-PUMA5CE, -SHAPE_AREA, -SHAPE_LEN, -ALAND, -AWATER)
st_write(cenacs1, dsn = "cty-sim/data", layer = "cenacs_2018",
         driver = "ESRI Shapefile", delete_layer = TRUE,
         overwrite = T)

## Test if it works
cenacs <- shapefile("cty-sim/data/cenacs_2018.shp")

#### Population raster ----
fl_hh <- raster(p2_hh)

cenacs_wgs <- spTransform(cenacs, CRS("+init=epsg:4326"))
cenacs_wgs@data <- cenacs_wgs@data %>%
  select(STATEFP10, COUNTYFP10, TRACTCE10, GEOID10)
fl_hh <- crop(fl_hh, cenacs_wgs)

writeRaster(fl_hh, "cty-sim/data/flhh10.tif", overwrite=T)

## Test if it works
fl_hh <- raster("cty-sim/data/flhh10.tif")
plot(fl_hh)
plot(cenacs_wgs, add=T)

#### Nursing home ----
nh_raw <- read_csv(p2_nh)
nh_sp <- SpatialPoints(nh_raw[,c("X", "Y")], CRS("+init=epsg:4326"))
cond <- !is.na(over(nh_sp, cenacs_wgs)$STATEFP10)
nh <- nh_raw[cond,]

write_csv(nh, "cty-sim/data/FL_Nursing_Homes.csv")

## Test
nh <- read_csv("cty-sim/data/FL_Nursing_Homes.csv")
nh

#### Schools ----
if (file.exists(p2_gcschx)) {
  gc_sch <- st_read(p2_gcschx)
} else {
  untar(p2_gcsch, exdir = "./tmp")
  gc_sch <- st_read("./tmp/gc_schools/gc_schools_sep17.shp")
}

cenacs_wgs_st <- st_as_sf(cenacs_wgs)
gc_sch_wgs <- gc_sch
gc_sch_wgs <- st_transform(gc_sch_wgs, 4326)
tmp <- st_join(gc_sch_wgs, cenacs_wgs_st)
cond <- !is.na(tmp$STATEFP10)
gc_sch <- gc_sch[cond,]

st_write(gc_sch, dsn = "cty-sim/data", layer = "gc_schools_sep17",
         driver = "ESRI Shapefile", delete_layer = TRUE,
         overwrite = T)

## College and University category has additional file
cu2 <- read_csv(p2_cuwsize) %>%
  filter(AUTOID %in% gc_sch$AUTOID)
write_csv(cu2, "cty-sim/data/cu_wsize.csv")


#### Workplace Area Characteristics ----
if (file.exists(p2_wacx)) {
  wac <- st_read(p2_wacx)
} else {
  untar(p2_wac, exdir = "./tmp")
  wac <- shapefile("./tmp/lehdwac_blk_2015/lehdwac_blk_2015.shp")
}

wac <- wac[wac$TRACTCE10 %in% cenacs$TRACTCE10,]
wac <- wac %>% dplyr::select(-SHAPE_AREA, -SHAPE_LEN)
st_write(wac, dsn = "cty-sim/data", layer = "lehdwac_blk_2015",
         driver = "ESRI Shapefile", delete_layer = TRUE,
         overwrite = T)

## Test if it works
wac <- shapefile("cty-sim/data/lehdwac_blk_2015.shp")

#### NCD Workplace data ----
if (file.exists(p2_ncdwpx)) {
  wp_coords <- read_csv(p2_ncdwpx)
} else {
  untar(p2_ncdwp, exdir = "./tmp")
  wp_coords <- read_csv("tmp/geocoded_workplaces_w_naics_w_essential.csv")
}

head(wp_coords)
wp_sf <- wp_coords[,c("x", "y")] %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)
wp_sf <- wp_sf %>%
  st_join(cenacs_wgs_st)
cond <- !is.na(wp_sf$STATEFP10)
wp_coords <- wp_coords[cond,]
write_csv(wp_coords, "cty-sim/data/geocoded_workplaces_w_naics_w_essential.csv")

#### HF data ----
hf <- read_tsv(p2_hf) %>%
  filter(!is.na(X))
hf_sf <- hf[,c("X", "Y")] %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)
hf_sf <- hf_sf %>%
  st_join(cenacs_wgs_st)

cond <- !is.na(hf_sf$STATEFP10)
hf1 <- hf[cond,]
write_csv(hf1, "cty-sim/data/hf_ahca.tsv")

#### BRFSS ----
# brfss <- fread(p2_brfss)
# brfss <- proc_brfss(brfss)
# brfss$`_IMPCTY` <- str_pad(brfss$`_IMPCTY`,
#                            width = 3, side = "left", pad = "0")
# brfss <- brfss %>%
#   filter(`_IMPCTY` == fips)
# write_csv(brfss, "cty-sim/data/brfss_proc.csv")
