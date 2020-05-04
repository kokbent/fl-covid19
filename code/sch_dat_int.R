rm(list = ls())

library(raster)
library(tidyverse)
library(Rcpp)

source("code/data_path.R")
sourceCpp("code/gravity.cpp")

#### Data import
## PERS
gen_pers <- read_csv("output/person_details.csv")
gen_hh <- read_csv("output/hh_coords.csv")

## SCH
if (dir.exists("./tmp/gc_schools/")) {
  gc_sch <- shapefile("./tmp/gc_schools/gc_schools_sep17.shp")
} else {
  untar(p2_gcsch, exdir = "./tmp")
  gc_sch <- shapefile("./tmp/gc_schools/gc_schools_sep17.shp")
}

gc_sch <- gc_sch %>%
  spTransform(CRS("+init=epsg:4326"))
head(gc_sch)
table(gc_sch$FLAG, useNA = "ifany")
table(gc_sch$TYPE, useNA = "ifany")

#### Cleaning data
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

#### Special considerations made for college/university
# For college/university, disable some which are non "classrooms"
cu <- gc_sch1 %>%
  filter(TYPE == "COLLEGE/UNIVERSITY") %>%
  arrange(NAME)
# write_csv(cu, "tmp/cu.csv") # Uncomment if cu_wsize.csv is not available
# And good luck filling in the "Population" of each cu, only 601!
cu1 <- read_csv("data/cu_wsize.csv") %>%
  dplyr::select(AUTOID, Population) %>%
  mutate(AUTOID = as.character(AUTOID))
cu <- cu %>%
  left_join(cu1)
gc_sch1 <- bind_rows(gc_sch1 %>% filter(TYPE != "COLLEGE/UNIVERSITY"), cu)

#### Assign students to schools
## Schools coordinates
sch_coord <- cbind(as.numeric(gc_sch$AUTOID), coordinates(gc_sch))
colnames(sch_coord) <- c("AUTOID", "x", "y")
sch_coord <- as.data.frame(sch_coord)

gc_sch2 <- gc_sch1 %>%
  select(AUTOID, AGELO, AGEHI, Population) %>%
  mutate(AUTOID = as.numeric(AUTOID)) %>%
  left_join(sch_coord)
gc_sch2$SID <- 1:nrow(gc_sch2)

## Coordinates of people who go to schools
sch_pers <- gen_pers %>%
  filter(SCHOOL == 2)
sch_pers <- sch_pers %>%
  left_join(gen_hh %>% select(HID, x, y))

table(sch_pers$AGE) # People up to 95 years old claim they're in school...

# Set artificial boundary of 35 as upper age limit for schooling.
sch_pers <- sch_pers %>%
  filter(AGE <= 35)
nrow(sch_pers) # Exclude about 300k...

# For each person in the dataframe, find out schools that match their age range,
# and choose ~ 50 nearest ones. Then assign them based on distance probability
sch_pers <- sch_pers %>%
  arrange(AGE, PID)
sch_pers1 <- as.data.frame(sch_pers[,c("x", "y", "AGE")]) %>% as.matrix
ages <- unique(sch_pers$AGE)
sid <- c()

for (a in 1:length(ages)) {
  age <- ages[a]
  print(age)
  
  xy_age <- sch_pers %>%
    filter(AGE == age) %>%
    select(x, y) %>%
    as.data.frame() %>%
    as.matrix()
  
  if (age >= 19) {
    sch_subs <- gc_sch2 %>%
      filter(AGELO <= age & AGEHI >= age) %>%
      filter(!is.na(Population))
    
    sch_subs_coord <- as.matrix(sch_subs[,c("x", "y")])
    
    system.time(
      sid_age <- assign_by_gravity(xy_age,
                                   sch_subs_coord,
                                   sch_subs$Population,
                                   50, 4326, steps = 4)
    ) %>% print()
  } else {
    sch_subs <- gc_sch2 %>%
      filter(AGELO <= age, AGEHI >= age)
    
    sch_subs_coord <- as.matrix(sch_subs[,c("x", "y")])
    
    system.time(
      sid_age <- assign_by_gravity(xy_age,
                                   sch_subs_coord,
                                   rep(1, nrow(sch_subs_coord)),
                                   50, 4326, steps = 4)
    ) %>% print()
    
    
  }
  
  sid_age2 <- sid_age[order(sid_age[,1]),]
  sid_age_v <- sch_subs$SID[sid_age2[,2]]
  
  sid <- c(sid, sid_age_v)
}

sch_pers$SID <- sid
sch_pers_simp <- sch_pers %>%
  select(PID, SID)
gen_pers <- gen_pers %>% 
  left_join(sch_pers_simp)
gen_pers <- gen_pers %>%
  select(PID, HID, NHID, SID, SEX, AGE, SCHOOL, EMPSTATD, PWSTATE2, PWPUMA00, GQ)
table(is.na(gen_pers$SID))

tmp <- sch_pers_simp %>%
  count(SID)

gc_sch3 <- gc_sch2 %>%
  select(SID, x, y) %>%
  left_join(tmp) %>%
  rename(STUDENT = n)
gc_sch3$WORKER <- ceiling(gc_sch3$STUDENT / 7)

write_csv(gc_sch3, "output/sch.csv")
write_csv(gen_pers, "output/person_details.csv")

png("fig/sch.png", 2400, 2400, pointsize = 10, res = 300)
plot(sch_pers[,c("x", "y")], pch = ".", asp = 1)
points(sch_pers[sch_pers$SID == 7565,c("x", "y")], pch = ".", col = "blue")
points(gc_sch2[,c("x", "y")], col = "red", pch=".")
dev.off()

# Santa Fe is 7442

# gen_hh <- read_csv("output/hh_coords.csv")
# sch_pers1 <- sch_pers %>%
#   left_join(gen_hh %>% select(HID, PUMA5CE))
# sum(sch_pers1$AGE[sch_pers1$PUMA5CE=="00101"] > 18)
# sum(sch_pers1$AGE[sch_pers1$PUMA5CE=="00102"] > 18)
# cond <- gc_sch1$COUNTY == "ALACHUA" & gc_sch1$TYPE == "COLLEGE/UNIVERSITY"
# gc_sch1[cond,]
