library(raster)
library(tidyverse)

source("code/data_path.R")

cenacs <- shapefile(fl_cenacs)
cnt <- shapefile(fl_cnt)
wac <- read_csv(fl_wac)

jobs_tract <- wac %>%
  group_by(TRACTCE10) %>%
  summarise(JOBS = sum(TOTAL_JOBS))
cenacs$TRACTCE10 %>% unique %>% length

wp_coords <- read.csv(fl_ncdwp, header = F)
wp_coords <- apply(wp_coords, 2, as.numeric) %>% as.data.frame
colnames(wp_coords) <- c("y", "x", "naics")

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

wp_sp <- SpatialPoints(wp_coords1[,c("x", "y")], proj4string = CRS("+init=epsg:4326"))
wp_sp <- wp_sp %>%
  spTransform(crs(cenacs))

cenacs2 <- cenacs
cenacs2@data <- cenacs@data[,c("TRACTCE10", "BLKGRPCE10")]
tmp <- over(wp_sp, cenacs2)
wp <- cbind(wp_coords1, tmp)

rm(cenacs2)
rm(tmp)

wp_sp <- wp_sp[!is.na(wp$TRACTCE10),]
windows()
plot(wp_sp, pch = ".", cex = .3, col = "blue")
plot(cnt, add=T)

wp <- wp %>%
  filter(!is.na(TRACTCE10))
nwp_jobs <- wp %>%
  group_by(TRACTCE10) %>%
  count() %>% left_join(jobs_tract) %>%
  filter(!is.na(TRACTCE10)) # Get rid of wp outside FL
plot(nwp_jobs$n, nwp_jobs$JOBS)

wp$size <- NA
for (i in 1:nrow(nwp_jobs)) {
  cond <- wp$TRACTCE10 == nwp_jobs$TRACTCE10[i]
  if (nwp_jobs$JOBS[i] <= nwp_jobs$n[i]) {
    wp$size[cond] <- 1
  } else {
    r <- nwp_jobs$JOBS[i] / nwp_jobs$n[i]
    y <- runif(nwp_jobs$n[i]*100) %>% matrix(nwp_jobs$n[i], 100)
    y <- 1/y^((r-1)/r)
    sel <- which.min(abs(colMeans(y) - r))
    y <- y[,sel]
    diff <- nwp_jobs$JOBS[i] - sum(round(y))
    y2 <- round(y*(1+diff/sum(y)))
    y2[y2==0] <- 1
    wp$size[cond] <- y2
  }
}

foo <- wp %>%
  group_by(TRACTCE10) %>%
  summarise(size = sum(size))
plot(foo$size, nwp_jobs$JOBS)

write_csv(wp, "tmp/wp_wo_schnh.csv")
