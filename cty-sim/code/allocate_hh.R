rm(list = ls())

#### Allocate households to the map & generate person data

#### Setting up ----
library(raster)
library(tidyverse)
library(data.table)
#library(tcltk)
library(doSNOW)
source("cty-sim/code/data_path.R")

ncore <- 4
cenacs <- shapefile(p2_cenacs)
fl_hh <- raster(p2_hh)
cenacs_wgs <- cenacs %>% spTransform(crs(fl_hh))

dat <- read_rds(p2_ipums)
hh_dat <- dat %>%
  select(YEAR, SERIAL, HHWT, PUMA, GQ) %>%
  distinct()

## Add PUMA to cenacs
# IPUMS has PUMA but shape has census tract, need census tract to PUMA conversion
ct_to_puma <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt")
ct_to_puma <- ct_to_puma %>%
  filter(STATEFP == "12") # FL is 12

cenacs@data <- cenacs@data %>%
  left_join(ct_to_puma[,c("COUNTYFP", "TRACTCE", "PUMA5CE")], 
            by = c("COUNTYFP10" = "COUNTYFP" ,"TRACTCE10" = "TRACTCE"))

## Calculate number of households needed in each PUMA
# Population weight is not consistent with household weight
# in IPUMS-ACS, so need to scale accordingly...

cenacs@data$HOUSEHOLDS <- as.numeric(cenacs@data$HOUSEHOLDS)
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

#### Generate hh coordinates based on census tract and gridded pop ----
# Using parallel for toy example here is a little overkill...
cl <- makeCluster(ncore)
registerDoSNOW(cl)

#pb <- tkProgressBar(max = nrow(cenacs))
#progress <- function(n) setTkProgressBar(pb, n)
#opts <- list(progress=progress)
hh_xy <- foreach(i = 1:nrow(cenacs), .packages = c("dplyr", "raster", "sp"), 
                 # .export=ls(envir=globalenv()), .options.snow = opts,
                 .export=ls(envir=globalenv()),
                 .combine = "rbind") %dopar% {
                   n_hh <- ceiling(cenacs$HOUSEHOLDS[i] * cenacs$scale[i])
                   N <- n_hh * 5
                   if (n_hh == 0) {
                     ret <- matrix(c(NA, NA, NA), 1, 3)
                   } else {
                     
                     repeat {
                       xy_i <- spsample(cenacs_wgs[i,], N, "random")
                       w <- raster::extract(fl_hh, xy_i)
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

#### Draw households from IPUMS and then plug into the HH coordinates ----
hh_xy <- na.omit(hh_xy)
hh_xy_df <- as.data.frame(hh_xy)
colnames(hh_xy_df) <- c("GEOID10", "x", "y")

hh_xy_puma <- cenacs@data %>%
  select(GEOID10, PUMA5CE, COUNTYFP10) %>%
  mutate(GEOID10 = as.numeric(GEOID10)) %>%
  right_join(hh_xy_df)
hh_xy_puma$YEAR <- hh_xy_puma$SERIAL <- NA

pumas <- unique(hh_xy_puma$PUMA5CE)

for (i in 1:length(pumas)) {
  # Sampling of household based on household weights
  print(i)
  cond <- hh_xy_puma$PUMA5CE == pumas[i]
  
  hh_samp <- hh_dat %>%
    filter(PUMA == as.numeric(pumas[i])) %>%
    sample_n(size = sum(cond), 
             replace = T,
             weight = HHWT)
  
  hh_xy_puma[cond,c("YEAR", "SERIAL")] <- hh_samp[,c("YEAR", "SERIAL")]
  
}  

#### Create person data based on hh data ----
hh_xy_puma$HID <- 1:nrow(hh_xy_puma)

dat_person <- dat %>%
  select(YEAR, SERIAL, SEX, AGE, SCHOOL, EMPSTATD, PWSTATE2, PWCOUNTY, PWPUMA00, GQ)
person_xy <- hh_xy_puma %>%
  left_join(dat_person)
person_xy$PID <- 1:nrow(person_xy)

#### Moving GQ == 3 and AGE >= 55 people to NH
nh_pop <- person_xy %>%
  filter(GQ == 3, AGE >= 55)
nh <- read_csv("cty-sim/output/nh.csv")

nh_pop$NHID <- NA
for (i in 1:length(pumas)) {
  print(i)
  cond <- nh_pop$PUMA5CE == pumas[i]
  
  if (sum(cond) == 0) next
  
  nh_samp <- nh %>%
    filter(PUMA5CE == pumas[i]) %>%
    sample_n(size = sum(cond), 
             replace = T,
             weight = REP_POP)
  
  nh_pop[cond,c("x", "y", "NHID")] <- nh_samp[,c("X", "Y", "NHID")]
}  

#### Reconcile nh_pop with generated hh and pers ----
# Bring nh_pop coordinates to hh_xy_puma 
# Bring nh_pop id to main pop df (ignore the coordinates since
# final pop df won't have coordinates)
hh_xy_puma[nh_pop$HID, c("x", "y")] <- nh_pop[, c("x", "y")]
person_xy <- person_xy %>% left_join(nh_pop %>% select(PID, NHID))

# Tabulate "population" of nh
nh_pop_count <- nh_pop %>%
  group_by(NHID) %>%
  count()
nh <- nh %>% left_join(nh_pop_count)
nh <- rename(nh, POP = n)

# Assign worker size to nh based on ratio 1:6 (with ceiling)
nh$WORKER <- ceiling(nh$POP / 6)
sum(nh$WORKER, na.rm = T)

#### Assign compliance score: for now at random
hh_xy_puma$compliance <- runif(nrow(hh_xy_puma))


#### Export
fwrite(hh_xy_puma %>% select(HID, x, y, SERIAL, PUMA5CE, COUNTYFP10, compliance),
       "cty-sim/output/hh_coords.csv")
fwrite(person_xy %>% select(PID, HID, NHID, SEX, AGE, SCHOOL, EMPSTATD, PWSTATE2, PWPUMA00, GQ),
       "cty-sim/output/person_details.csv")
fwrite(nh %>% select(NHID, x = X, y = Y, WORKER, POP, PUMA5CE, REP_POP),
       "cty-sim/output/nh.csv")
