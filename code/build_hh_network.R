rm(list = ls())
library(tidyverse)
library(Rcpp)

sourceCpp("code/action_by_gravity.cpp")

#### Build household network
con <- RSQLite::dbConnect(RSQLite::SQLite(), "output/gen_dat/gen_dat.sqlite")
sql <- "SELECT hh.locid, hh.hid, hh.pop, hh.nh, loc.x, loc.y FROM hh 
LEFT JOIN loc
ON loc.locid = hh.locid
WHERE hh.nh = 'n'"
hh_nonnh <- RSQLite::dbGetQuery(con, sql)

sum(is.na(hh_nonnh$n))

## Determine the "capacity" of each household
## Rules: (1) Household mean = HH member size
set.seed(1849)
hh_nonnh$cap <- rpois(nrow(hh_nonnh), hh_nonnh$pop)
sum(hh_nonnh$cap == 0)
hh_wcap <- hh_nonnh %>%
  filter(cap != 0)

## Take subset for a spin
hh_sub <- hh_wcap[1:10000,] %>% as.data.frame()
# hh_sub <- read_csv("./tmp/tmp_hhsub.csv") %>% as.data.frame()
system.time(
  edges <- build_network(locs = hh_sub[,c("x", "y")] %>% as.matrix(),
                         weights = hh_sub$cap, 1000, seed = 4326)
)

m <- as.vector(table(edges))
plot(hh_sub$cap, m)
sum(m != hh_sub$cap)

## Run away with it!
system.time(
  edges <- build_network(locs = hh_wcap[,c("x", "y")] %>% as.matrix(),
                         weights = hh_wcap$cap, 1000, seed = 4342024)
)

locid1 <- hh_wcap$locid[edges[,1]]
locid2 <- hh_wcap$locid[edges[,2]]

mean(locid1 == locid2)
edges_locid <- cbind(locid1, locid2) - 1

## Export
# write_csv(as.data.frame(edges), "./tmp/edges.csv")
write_delim(as.data.frame(edges_locid), "./output/network-florida.txt", col_names = F)
file.copy("./output/network-florida.txt", "sim_pop-florida/network-florida.txt")
