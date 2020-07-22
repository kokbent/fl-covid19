rm(list = ls())
library(tidyverse)
library(Rcpp)
library(data.table)

sourceCpp("code/action_by_gravity.cpp")

#### Build household network
pers <- fread("toy2/output/pers_w_wid.csv")
hh <- fread("toy2/output/hh_coords.csv")
nh <- fread("toy2/output/nh.csv")

hh_nh <- inner_join(hh, nh %>% select(x, y))
hh_nonnh <- hh %>%
  filter(!HID %in% hh_nh$HID)

## Determine HH size
hh_count <- pers %>%
  group_by(HID) %>%
  count()

hh_nonnh <- hh_nonnh %>%
  left_join(hh_count)

sum(hh_nonnh$n)

## Determine the "capacity" of each household
## Rules: (1) Household mean = HH member size
set.seed(1849)
hh_nonnh$cap <- rpois(nrow(hh_nonnh), hh_nonnh$n)
sum(hh_nonnh$cap == 0)
hh_wcap <- hh_nonnh %>%
  filter(cap != 0)

## Run away with it!
system.time(
  edges <- build_network(locs = hh_wcap[,c("x", "y")] %>% as.matrix(),
                         weights = hh_wcap$cap, 1000, seed = 4342024)
)

hid1 <- hh_wcap$HID[edges[,1]]
hid2 <- hh_wcap$HID[edges[,2]]

mean(hid1 == hid2)
edges_hid <- cbind(HID1 = hid1, HID2 = hid2) %>% as.data.table

## Export
# write_csv(as.data.frame(edges), "./tmp/edges.csv")
# write_delim(as.data.frame(edges_hid), "./output/network-florida.txt", col_names = F)
# file.copy("./output/network-florida.txt", "sim_pop-florida/network-florida.txt")
fwrite(edges_hid, "toy2/output/hh_network.csv")
