rm(list=ls())

library(tidyverse)

# Process data into workable data for models
wp <- read_csv("output/wp.csv")
head(wp)
colnames(wp) <- tolower(colnames(wp))

hh <- read_csv("output/hh_coords.csv")
head(hh)
colnames(hh) <- tolower(colnames(hh))

nh <- read_csv("output/nh.csv")
head(nh)
colnames(nh) <- tolower(colnames(nh))
nh <- nh %>%
  select(nhid, pop) %>%
  left_join(wp %>% filter(type == "n"), by = c("nhid" = "serial")) %>%
  select(nhid, x, y, pop, worker, wid)

sch <- read_csv("output/sch.csv")
head(sch)
colnames(sch) <- tolower(colnames(sch))
sch <- sch %>%
  select(sid, student) %>%
  left_join(wp %>% filter(type == "s"), by = c("sid" = "serial")) %>%
  select(sid, x, y, student, worker, wid)

pers <- read_csv("output/person_details.csv")
head(pers)
colnames(pers) <- tolower(colnames(pers))

# pull out essential columns
wp_e <- wp %>%
  select(wid, type, x, y, worker)

hh_e <- hh %>%
  select(hid, x, y)

nh_e <- nh %>%
  select(nhid, wid, x, y, pop, worker)

sch_e <- sch %>%
  select(sid, wid, x, y, pop = student, worker)

pers_e <- pers %>%
  mutate(wid = NA) %>%
  select(pid, hid, wid, nhid, sid, sex, age, empstat = empstatd)

dir.create("gen_dat")
write_delim(wp_e, "gen_dat/wp.txt")
write_delim(hh_e, "gen_dat/hh.txt")
write_delim(nh_e, "gen_dat/nh.txt")
write_delim(sch_e, "gen_dat/sch.txt")
write_delim(pers_e, "gen_dat/pers.txt")

file.copy("output/output_zip_metadata.txt", "gen_dat/.")
tar("output/gen_dat.tgz", files = "gen_dat/", compression = "gzip")
unlink("./gen_dat", recursive = T)
