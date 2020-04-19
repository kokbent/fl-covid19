rm(list=ls())

library(raster)
library(tidyverse)

source("code/data_path.R")

untar(fl_cenacs, exdir = "./tmp/")
# untar(fl_wac, exdir = "./tmp/")
# wac <- shapefile("tmp/lehdwac_blk_2015/lehdwac_blk_2015.shp")
# wac <- wac@data
# write_csv(wac, "data/wac.csv")
cenacs <- shapefile("tmp/cenacs/cenacs_2018.shp")
wac <- read_csv("data/wac.csv")

jobs_tract <- wac %>%
  group_by(TRACTCE10) %>%
  summarise(JOBS = sum(TOTAL_JOBS))
cenacs$TRACTCE10 %>% unique %>% length

# Untar
untar(p2_ncdwp, exdir = "./tmp")
wp_coords <- read_csv("tmp/geocoded_workplaces_w_naics.csv")
file.remove("tmp/geocoded_workplaces_w_naics.csv")

# Think...
wp_unique <- wp_coords %>%
  dplyr::filter(!is.na(x), !is.na(y)) %>%
  group_by(x, y, naics) %>%
  count() %>%
  arrange(desc(n))

naics_unique <- table(wp_unique$naics, wp_unique$n > 1)
tmp <- as.numeric(row.names(naics_unique)) 
ind <- which(tmp >= 611110 & tmp <= 611699)
naics_unique[ind,]

# Handle Schools and Nursing Home separately
# Note that there's general mismatch in number of schools and nursing home among
# data sources...
wp_coords1 <- wp_coords %>%
  dplyr::filter(!is.na(x), !is.na(y)) %>%
  dplyr::filter(!(naics >= 611110 & naics <= 611399) | is.na(naics)) %>%
  dplyr::filter(!naics %in% c(623312, 623110) | is.na(naics))
wp_coords1$WORKER <- NA
wp_coords1 <- wp_coords1 %>%
  select(SERIAL = serial, x, y, WORKER) %>%
  mutate(TYPE = "w")

# Add NH and SCH
nh <- read_csv("output/nh.csv") %>%
  mutate(SERIAL = NHID, TYPE = "n") %>%
  select(SERIAL, x, y, WORKER, TYPE)
sch <- read_csv("output/sch.csv") %>%
  mutate(SERIAL = SID, TYPE = "s") %>%
  select(SERIAL, x, y, WORKER, TYPE)

wp_coords2 <- bind_rows(wp_coords1, nh, sch)
head(wp_coords2)

wp_sp <- SpatialPoints(wp_coords2[,c("x", "y")], proj4string = CRS("+init=epsg:4326"))
wp_sp <- wp_sp %>%
  spTransform(crs(cenacs))

cenacs2 <- cenacs
cenacs2@data <- cenacs@data[,c("TRACTCE10", "BLKGRPCE10")]
tmp <- over(wp_sp, cenacs2)
wp <- cbind(wp_coords2, tmp)

rm(cenacs2)
rm(tmp)

# wp_sp <- wp_sp[!is.na(wp$TRACTCE10),]
# windows()
# plot(wp_sp, pch = ".", cex = .3, col = "blue")
# plot(cnt, add=T)

wp <- wp %>%
  filter(!is.na(TRACTCE10)) # Get rid of wp outside FL
nwp_jobs <- wp %>%
  group_by(TRACTCE10) %>%
  summarise(WP_TOFILL = sum(is.na(WORKER)), 
            CURR_JOBS = sum(WORKER, na.rm = T)) %>% 
  left_join(jobs_tract) %>%
  mutate(JOBS_TOFILL = JOBS - CURR_JOBS,
         JOBS_TOFILL = ifelse(JOBS_TOFILL<0, 0, JOBS_TOFILL))

wp1 <- wp
for (i in 1:nrow(nwp_jobs)) {
  cond <- wp1$TRACTCE10 == nwp_jobs$TRACTCE10[i] & is.na(wp1$WORKER)
  if (nwp_jobs$JOBS_TOFILL[i] <= nwp_jobs$WP_TOFILL[i]) {
    wp1$WORKER[cond] <- 1
  } else {
    # Create 100 sets random pareto numbers by transform from unif
    r <- nwp_jobs$JOBS_TOFILL[i] / nwp_jobs$WP_TOFILL[i]
    y <- runif(nwp_jobs$WP_TOFILL[i]*100) %>% matrix(nwp_jobs$WP_TOFILL[i], 100)
    y <- 1/y^((r-1)/r) # Transform to pareto from uniform samples
    
    # Mean of pareto is unstable, so choose sample set with mean closest to ideal
    sel <- which.min(abs(colMeans(y) - r))
    y <- y[,sel]
    
    # Scale to ideal mean
    diff <- nwp_jobs$JOBS_TOFILL[i] - sum(round(y))
    y2 <- round(y*(1+diff/sum(y)))
    y2[y2==0] <- 1
    wp1$WORKER[cond] <- y2
  }
}

foo <- wp1 %>%
  group_by(TRACTCE10) %>%
  summarise(WORKER = sum(WORKER))
plot(log(foo$WORKER), log(nwp_jobs$JOBS))

wp1$WID <- 1:nrow(wp1)
wp2 <- wp1 %>%
  select(WID, TYPE, x, y, WORKER, SERIAL)

write_csv(wp2, "output/wp.csv")
