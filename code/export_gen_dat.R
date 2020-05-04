#### Post processing of data
rm(list=ls())

library(tidyverse)

#### Data import
wp <- read_csv("output/wp2.csv")
head(wp)
colnames(wp) <- tolower(colnames(wp))

hh <- read_csv("output/hh_coords.csv")
head(hh)
colnames(hh) <- tolower(colnames(hh))

nh <- read_csv("output/nh.csv")
head(nh)
colnames(nh) <- tolower(colnames(nh))
nh <- nh %>%
  select(nhid, x, y, pop, rep_pop) %>%
  left_join(wp %>% filter(type == "n"), by = c("x", "y")) %>%
  select(nhid, wid2, x, y, pop, worker, rep_pop)

sch <- read_csv("output/sch.csv")
head(sch)
colnames(sch) <- tolower(colnames(sch))
sch <- sch %>%
  select(sid, x, y, student) %>%
  left_join(wp %>% filter(type == "s"), by = c("x", "y")) %>%
  select(sid, x, y, student, worker, wid2)

pers <- read_csv("output/pers_w_wid.csv")
head(pers)
colnames(pers) <- tolower(colnames(pers))

#### Build big location table
## Remove households in NH (for location file)
pers_nh <- pers %>%
  filter(!is.na(nhid))
hh_nonnh <- hh %>%
  filter(!hid %in% pers_nh$hid)

## Bind WP with HH_NONNH (WP already contains SCH and NH)
table(wp$type)
loc <- wp %>%
  select(wid2, x, y, type)
loc <- bind_rows(loc, hh_nonnh %>% mutate(type = "h") %>% select(hid, x, y, type))
table(loc$type)

loc$locid <- 1:nrow(loc)

# LOC acts as the "Master Table" here; keeping WID2 for now
table(is.na(loc$wid2))

#### Table for each location type; connect them with LOC keep original ID for reference
## SCH
sch <- sch %>%
  left_join(loc %>% 
              filter(type == "s") %>%
              select(wid2, locid))
sch_db <- sch %>%
  select(locid, pop = student, worker, sid) %>%
  mutate(essential = "n")

## NH
nh <- nh %>%
  left_join(loc %>% 
              filter(type == "n") %>%
              select(wid2, locid))
nh_db <- nh %>%
  select(locid, pop, worker, nhid) %>%
  mutate(essential = "y")

## WP (exclude SCH & NH)
wp <- wp %>%
  filter(type == "w") %>%
  left_join(loc %>% 
              filter(type == "w") %>%
              select(wid2, locid))
wp_db <- wp %>%
  select(locid, wid2, worker, naics, essential = ess_class) %>%
  mutate(essential = case_when(
    is.na(essential) ~ "n",
    essential == "Essential" ~ "y",
    essential == "Essential*" ~ "y",
    essential == "Nonessential" ~ "n"
  ))

## HH
pers_nh <- pers_nh %>%
  left_join(nh_db %>% select(nhid, locid)) %>%
  left_join(hh %>% select(hid, serial))
hh_nh <- pers_nh %>%
  select(locid, hid, serial) %>%
  group_by(hid) %>%
  mutate(pop = n(),
         nh = "y")
hh_nonnh <- hh_nonnh %>%
  left_join(pers %>%
              group_by(hid) %>%
              summarise(pop = n()))
hh_nonnh <- hh_nonnh %>%
  left_join(loc %>% 
              filter(type == "h") %>%
              select(hid, locid)) %>%
  mutate(nh = "n")
hh_db <- bind_rows(hh_nh, hh_nonnh %>% select(locid, hid, serial, pop, nh))

## PERS
pers_db <- pers %>%
  select(pid, hid, sex, age, school, empstatd, pwstate2, pwpuma00, gq)

#### MOVEMENT
## STUDY
study_db <- pers %>%
  select(pid, sid) %>%
  filter(!is.na(sid))
study_db <- study_db %>% left_join(sch_db %>% select(sid, locid)) %>%
  select(pid, locid) %>%
  mutate(type = "s")

## WORK
work_db <- pers %>%
  select(pid, wid2) %>%
  filter(!is.na(wid2)) %>%
  left_join(wp_db %>% select(wid2, locid)) %>%
  select(pid, locid) %>%
  mutate(type = "w")

## Combined movement
movement_db <- bind_rows(study_db, work_db)
rm(study_db)
rm(work_db)

#### RESIDENCE
## RESIDE_NH
reside_nh_db <- pers_nh %>%
  select(pid, locid) %>%
  mutate(type = "n")

## RESIDE_HH
reside_hh_db <- pers_nh %>%
  select(pid, locid) %>%
  mutate(type = "n")

reside_hh_db <- inner_join(pers %>% filter(is.na(nhid)), 
                           hh_db %>% select(locid, hid)) %>%
  select(pid, locid) %>%
  mutate(type = "h")

## Combine Reside
reside_db <- bind_rows(reside_nh_db, reside_hh_db)
rm(reside_nh_db)
rm(reside_hh_db)

#### Build DB
library(dbplyr)
library(RSQLite)

dir.create("gen_dat")
mydb <- dbConnect(RSQLite::SQLite(), "gen_dat/gen_dat.sqlite")
dbWriteTable(mydb, "loc", loc)
dbWriteTable(mydb, "pers", pers_db)
dbWriteTable(mydb, "hh", hh_db)
dbWriteTable(mydb, "sch", sch_db)
dbWriteTable(mydb, "nh", nh_db)
dbWriteTable(mydb, "wp", wp_db)
dbWriteTable(mydb, "movement", movement_db)
dbWriteTable(mydb, "reside", reside_db)
dbDisconnect(mydb)

tar("output/gen_dat.tgz", files = "gen_dat/", compression = "gzip")
unlink("./gen_dat", recursive = T)

# dir.create("gen_dat")
# write_delim(wp_e, "gen_dat/wp.txt")
# write_delim(hh_e, "gen_dat/hh.txt")
# write_delim(nh_e, "gen_dat/nh.txt")
# write_delim(sch_e, "gen_dat/sch.txt")
# write_delim(pers_e, "gen_dat/pers.txt")
# 
# file.copy("output/output_zip_metadata.txt", "gen_dat/.")
# tar("output/gen_dat.tgz", files = "gen_dat/", compression = "gzip")
# unlink("./gen_dat", recursive = T)
