rm(list = ls())

library(raster)
library(tidyverse)
library(tcltk)
library(doSNOW)

source("code/data_path.R")

cenacs <- shapefile(fl_cenacs)
fl_cnt_shp <- shapefile(fl_cnt)
fl_hh_r <- raster(fl_hh)
cenacs_wgs <- cenacs %>% spTransform(crs(fl_hh_r))

dat <- read_rds(ipums_file)
hh_dat <- dat %>%
  select(YEAR, SERIAL, HHWT, PUMA) %>%
  distinct()

# IPUMS has PUMA but shape has census tract
# Need census tract to PUMA conversion
ct_to_puma <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt")
ct_to_puma <- ct_to_puma %>%
  filter(STATEFP == "12") # FL is 12

# conversion table matches with cenacs
mean(cenacs$TRACTCE10 %in% ct_to_puma$TRACTCE)
mean(ct_to_puma$TRACTCE %in% cenacs$TRACTCE10)

cenacs@data <- cenacs@data %>%
  left_join(ct_to_puma[,c("COUNTYFP", "TRACTCE", "PUMA5CE")], 
            by = c("COUNTYFP10" = "COUNTYFP" ,"TRACTCE10" = "TRACTCE"))
cenacs@data$HOUSEHOLDS <- as.numeric(cenacs@data$HOUSEHOLDS)

# cenacs reported different number of households from IPUMS-ACS
# scaling cenacs number up to match IPUMS-ACS (Done by PUMA)
# cenacs_hh <- cenacs@data %>%
#   group_by(PUMA5CE) %>%
#   summarise(HH = sum(HOUSEHOLDS))
# ipums_hh <- hh_dat %>%
#   group_by(PUMA) %>%
#   summarise(HH = sum(HHWT))
# cenacs_hh$scale <- ipums_hh$HH / cenacs_hh$HH
# cenacs@data <- cenacs@data %>% left_join(cenacs_hh)

# Also, population weight is not consistent with household weight
# in IPUMS-ACS, so need to scale accordingly...
cenacs_hh <- cenacs@data %>%
  group_by(PUMA5CE) %>%
  summarise(HH = sum(HOUSEHOLDS),
            POP = sum(as.numeric(TOTALPOP)))
ipums_hh <- dat %>%
  group_by(PUMA) %>%
  summarise(POP = sum(PERWT),
            PW_INF = sum(PERWT) / sum(HHWT))
ipums_hh2 <- hh_dat %>%
  group_by(PUMA) %>%
  summarise(HH = sum(HHWT))
ipums_hh$HH_need <- ipums_hh2$HH * ipums_hh$PW_INF

cenacs_hh$scale <- ipums_hh$HH_need / cenacs_hh$HH
cenacs@data <- cenacs@data %>% left_join(cenacs_hh %>% select(PUMA5CE, scale))


# Generate hh coordinates based on census tract and gridded pop
cl <- makeCluster(5)
registerDoSNOW(cl)

pb <- tkProgressBar(max = nrow(cenacs))
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress=progress)

hh_xy <- foreach(i = 1:nrow(cenacs), .packages = c("dplyr", "raster", "sp"), 
                 .export=ls(envir=globalenv()), .options.snow = opts,
                 .combine = "rbind") %dopar% {
                   n_hh <- ceiling(cenacs$HOUSEHOLDS[i] * cenacs$scale[i])
                   N <- n_hh * 5
                   if (n_hh == 0) {
                     ret <- matrix(c(NA, NA, NA), 1, 3)
                   } else {
                     
                     repeat {
                       xy_i <- spsample(cenacs_wgs[i,], N, "random")
                       w <- raster::extract(fl_hh_r, xy_i)
                       cond <- !is.na(w)
                       xy_i <- xy_i[cond,]
                       w <- w[cond]
                       
                       if (sum(w > 0) >= n_hh * 2) break
                       else {
                         N <- N + n_hh * 2
                         print(N)
                       }
                       
                     }
                     
                     sel <- sample(1:length(xy_i), n_hh, F, w)
                     ret <- cbind(as.numeric(cenacs$GEOID10[i]), coordinates(xy_i)[sel,])
                   }
                   
                   ret
                 }

stopCluster(cl)

# Draw households from IPUMS ACS data based on PUMA
hh_xy <- na.omit(hh_xy)
hh_xy_df <- as.data.frame(hh_xy)
colnames(hh_xy_df) <- c("GEOID10", "x", "y")

hh_xy_puma <- cenacs@data %>%
  select(GEOID10, PUMA5CE) %>%
  mutate(GEOID10 = as.numeric(GEOID10)) %>%
  right_join(hh_xy_df)
hh_xy_puma$YEAR <- hh_xy_puma$SERIAL <- NA

pumas <- unique(hh_xy_puma$PUMA5CE)

for (i in 1:length(pumas)) {
  print(i)
  cond <- hh_xy_puma$PUMA5CE == pumas[i]
  
  hh_samp <- hh_dat %>%
    filter(PUMA == as.numeric(pumas[i])) %>%
    sample_n(size = sum(cond), 
             replace = T,
             weight = HHWT)
  
  hh_xy_puma[cond,c("YEAR", "SERIAL")] <- hh_samp[,c("YEAR", "SERIAL")]
  
}  

# Create person data based on hh data
hh_xy_puma$HID <- 1:nrow(hh_xy_puma)

dat_person <- dat %>%
  select(YEAR, SERIAL, SEX, AGE, SCHOOL, EMPSTATD, PWSTATE2, PWCOUNTY, PWPUMA00)
person_xy <- hh_xy_puma %>%
  left_join(dat_person)
person_xy$PID <- 1:nrow(person_xy)

write_csv(hh_xy_puma %>% select(HID, x, y),
          "output/hh_coords.csv")
write_csv(person_xy %>% select(PID, HID, x, y, SEX, AGE, SCHOOL, EMPSTATD, PWSTATE2, PWPUMA00),
          "output/person_details.csv")

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
