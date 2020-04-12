rm(list = ls())

library(raster)
library(tidyverse)
library(future)
library(furrr)
library(tcltk)

source("code/data_path.R")

gc_sch <- shapefile(fl_gcsch)
head(gc_sch)
table(gc_sch$FLAG, useNA = "ifany")
table(gc_sch$TYPE, useNA = "ifany")

# Use GCSCH data for school locations (excl ADULT, SUPPORT SERVICES, UNKNOWN, UNASSIGNED)
# remove corresponding ncd entries based on naics
gc_sch1 <- gc_sch@data %>%
  filter(FLAG == "V") %>%
  filter(!TYPE %in% c("ADULT", "DISTRICT OFFICE (SCHOOL BOARD)",
                      "SUPPORT SERVICES", "UNASSIGNED", "UNKNOWN"))
table(gc_sch1$TYPE, useNA = "ifany")

# Assign the age range of schools based on type
gc_sch1 <- gc_sch1 %>%
  mutate(AGELO = case_when(
    TYPE == "COLLEGE/UNIVERSITY" ~ 19,
    TYPE == "COMBINATION ELEMENTARY & MIDDLE" ~ 6,
    TYPE == "COMBINATION ELEMENTARY & SECONDARY" ~ 6,
    TYPE == "COMBINATION JR. HIGH & SENIOR HIGH" ~ 12,
    TYPE == "ELEMENTARY" ~ 6,
    TYPE == "HEAD START" ~ 3,
    TYPE == "KINDERGARTEN" ~ 3,
    TYPE == "LEARNING CENTER" ~ 3,
    TYPE == "MIDDLE/JR. HIGH" ~ 12,
    TYPE == "PRE-KINDERGARTEN" ~ 3,
    TYPE == "PRE-KINDERGARTEN-KINDERGARTEN" ~ 3,
    TYPE == "SENIOR HIGH" ~ 16
  ))

gc_sch1 <- gc_sch1 %>%
  mutate(AGEHI = case_when(
    TYPE == "COLLEGE/UNIVERSITY" ~ 35,
    TYPE == "COMBINATION ELEMENTARY & MIDDLE" ~ 15,
    TYPE == "COMBINATION ELEMENTARY & SECONDARY" ~ 18,
    TYPE == "COMBINATION JR. HIGH & SENIOR HIGH" ~ 18,
    TYPE == "ELEMENTARY" ~ 12,
    TYPE == "HEAD START" ~ 5,
    TYPE == "KINDERGARTEN" ~ 5,
    TYPE == "LEARNING CENTER" ~ 5,
    TYPE == "MIDDLE/JR. HIGH" ~ 15,
    TYPE == "PRE-KINDERGARTEN" ~ 5,
    TYPE == "PRE-KINDERGARTEN-KINDERGARTEN" ~ 5,
    TYPE == "SENIOR HIGH" ~ 18
  ))

# For college/university, disable some which are non "classrooms"
cu <- gc_sch1 %>%
  filter(TYPE == "COLLEGE/UNIVERSITY") %>%
  group_by(OWNER) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(desc(n), OWNER)
cu

disab_ind <- c(1:7, 10:15, 17:27, 33:35, 47, 57, 60, 118, 119, 129, 137, 144, 159, 168, 170, 183)
name <- cu$NAME[disab_ind]
gc_sch1$AGELO[gc_sch1$NAME %in% name] <- 0
gc_sch1$AGEHI[gc_sch1$NAME %in% name] <- 0

#### Assign students to schools
## Schools coordinates
sch_coord <- cbind(as.numeric(gc_sch$AUTOID), coordinates(gc_sch))
colnames(sch_coord) <- c("AUTOID", "x", "y")
sch_coord <- as.data.frame(sch_coord)

gc_sch2 <- gc_sch1 %>%
  select(AUTOID, AGELO, AGEHI) %>%
  mutate(AUTOID = as.numeric(AUTOID)) %>%
  left_join(sch_coord)
gc_sch2$SID <- 1:nrow(gc_sch2)

## Generated persons coordinates
gen_pers <- read_csv("output/person_details.csv")
sch_pers <- gen_pers %>%
  filter(SCHOOL == 2)
gen_hh <- read_csv("output/hh_coords.csv")
sch_pers <- sch_pers %>%
  left_join(gen_hh %>% select(HID, x, y))
rm(gen_hh)

tmp <- SpatialPoints(sch_pers[,c("x", "y")], proj4string = CRS("+init=epsg:4326")) %>%
  spTransform(crs(gc_sch))
sch_pers[,c("x", "y")] <- coordinates(tmp)

table(sch_pers$AGE) # People up to 95 years old claim they're in school...

# Set artificial boundary of 35 as upper age limit for schooling.
sch_pers <- sch_pers %>%
  filter(AGE <= 35)
nrow(sch_pers) # Exclude about 300k...

# For each person in the dataframe, find out schools that match their age range,
# and choose ~ 50 nearest ones. Then assign them based on distance probability
assign_sid <- function (ind, xy_age, sch_subs, cand_dist) {
  xy <- xy_age[ind,,drop=F]
  sch_subs$dx <- abs(sch_subs$x - xy[1])
  sch_subs$dy <- abs(sch_subs$y - xy[2])
  
  nsch_dist <- sapply(cand_dist, 
                      function (x) (rowSums(sch_subs[,c("dx", "dy")] <= x) == 2) %>% sum)
  ind1 <- which(nsch_dist >= 50)
  ind1 <- ifelse(length(ind1) == 0, 6, min(ind1))
  
  sch_subs <- sch_subs %>%
    filter(dx <= cand_dist[ind1], dy <= cand_dist[ind1])
  d <- Rfast::dista(xy, sch_subs[,c("x", "y")]) %>% as.vector
  
  sel <- sample(1:nrow(sch_subs), 1, prob = 1/d)
  return(sch_subs$SID[sel])
}

sch_pers <- sch_pers %>%
  arrange(AGE, PID)
sch_pers1 <- as.data.frame(sch_pers[,c("x", "y", "AGE")]) %>% as.matrix
cand_dist <- 1:6 * 25000
ages <- unique(sch_pers$AGE)
sid <- c()
plan(multiprocess(workers = 8))

for (a in 1:length(ages)) {
  age <- ages[a]
  print(age)
  
  xy_age <- sch_pers %>%
    filter(AGE == age) %>%
    select(x, y) %>%
    as.data.frame() %>%
    as.matrix()
  sch_subs <- gc_sch2 %>%
    filter(AGELO <= age, AGEHI >= age)
  
  sid_age <- future_map_int(1:nrow(xy_age), ~ assign_sid(.x, xy_age, sch_subs, cand_dist),
                            .progress = T)  
  sid <- c(sid, sid_age)
}
plan(sequential)

sch_pers$SID <- sid
sch_pers_simp <- sch_pers %>%
  select(PID, SID)
gen_pers <- gen_pers %>% left_join(sch_pers_simp)
gen_pers <- gen_pers %>%
  select(PID, HID, NHID, SID, SEX, AGE, SCHOOL, EMPSTATD, PWSTATE2, PWPUMA00, GQ)
table(is.na(gen_pers$SID))

tmp <- sch_pers_simp %>%
  count(SID)

gc_sch3 <- gc_sch2 %>%
  select(SID, x, y) %>%
  left_join(tmp) %>%
  rename(STUDENT = n)
gc_sch3[,c("x", "y")] <- SpatialPoints(gc_sch3[,c("x", "y")], proj4string = crs(gc_sch)) %>%
  spTransform(CRS("+init=epsg:4326")) %>%
  coordinates
gc_sch3$WORKER <- ceiling(gc_sch3$STUDENT / 7)

write_csv(gc_sch3, "output/sch.csv")
write_csv(gen_pers, "output/person_details.csv")

png("fig/sch.png", 2400, 2400, pointsize = 10, res = 300)
plot(sch_pers[,c("x", "y")], pch = ".", asp = 1)
points(sch_pers[sch_pers$SID == 3055,c("x", "y")], pch = ".", col = "blue")
points(gc_sch2[,c("x", "y")], col = "red", pch=".")
dev.off()

# gen_hh <- read_csv("output/hh_coords.csv")
# sch_pers1 <- sch_pers %>%
#   left_join(gen_hh %>% select(HID, PUMA5CE))
# sum(sch_pers1$AGE[sch_pers1$PUMA5CE=="00101"] > 18)
# sum(sch_pers1$AGE[sch_pers1$PUMA5CE=="00102"] > 18)
# cond <- gc_sch1$COUNTY == "ALACHUA" & gc_sch1$TYPE == "COLLEGE/UNIVERSITY"
# gc_sch1[cond,]
