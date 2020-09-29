rm(list = ls())

library(sp)
library(sf)
library(tidyverse)
library(data.table)
library(RSQLite)
library(geosphere)

source("code/data_path.R")

con <- DBI::dbConnect(RSQLite::SQLite(), "sim_pop-florida-1.1/sim_pop-florida-1.1.sqlite")

sql <- "SELECT m.pid AS pid, m.locid AS locid, loc.x AS lx, loc.y AS ly 
FROM movement AS m 
LEFT JOIN loc
ON m.locid = loc.locid
WHERE m.type = 'w'"

df <- dbGetQuery(con, sql)

sql <- "SELECT pers.pid, hh.locid, loc.x AS hx, loc.y AS hy FROM pers
LEFT JOIN hh
ON pers.hid = hh.hid
LEFT JOIN loc
ON hh.locid = loc.locid"

df2 <- dbGetQuery(con, sql)

df <- df %>%
  select(pid, lx, ly) %>%
  left_join(df2)

ind1 <- sample(1:nrow(df), ceiling(0.005 * nrow(df)))
dfsub <- df[ind1,c("hx", "hy", "lx", "ly")]
linelist <- apply(dfsub, 1, function (x) Line(rbind(x[1:2], x[3:4])))
linelist <- Lines(linelist, ID = 1)
linelist <- SpatialLines(list(linelist))
linelist

ll <- st_as_sf(linelist) %>%
  st_set_crs(4326)
fl_cnt_shp <- st_read(p2_cntx)
fl_cnt_shp <- st_transform(fl_cnt_shp, crs = 4326)

p1 <- ggplot() +
  geom_sf(data = fl_cnt_shp) +
  geom_sf(data = ll, colour = "#0000FF20") +
  labs(title = "Movement between Homes and Workplaces") +
  theme_bw()

ggsave("fig/workdist_1.1b.png", p1, dpi = 320, width = 20, height = 20, units = "cm")

# png("fig/workdist_1.1.png", 1200, 1600)
# plot(linelist, col = "#00000010")
# dev.off()


#### School trav
sql <- "SELECT m.pid AS pid, m.locid AS locid, loc.x AS lx, loc.y AS ly 
FROM movement AS m 
LEFT JOIN loc
ON m.locid = loc.locid
WHERE m.type = 's'"

df <- dbGetQuery(con, sql)

sql <- "SELECT pers.pid, hh.locid, loc.x AS hx, loc.y AS hy FROM pers
LEFT JOIN hh
ON pers.hid = hh.hid
LEFT JOIN loc
ON hh.locid = loc.locid"

df2 <- dbGetQuery(con, sql)

df <- df %>%
  select(pid, lx, ly) %>%
  left_join(df2)

ind <- sample(1:nrow(df), ceiling(0.01 * nrow(df)))
system.time(distances <- apply(df[ind,c("hx", "hy", "lx", "ly")], 1,
                               function (x) distHaversine(x[1:2], x[3:4])))
plot(density(distances/1000, na.rm=T))
summary(distances/1000)

ind1 <- sample(1:nrow(df), ceiling(0.005 * nrow(df)))
dfsub <- df[ind1,c("hx", "hy", "lx", "ly")]
linelist <- apply(dfsub, 1, function (x) Line(rbind(x[1:2], x[3:4])))
linelist <- Lines(linelist, ID = 1)
linelist <- SpatialLines(list(linelist))
png("fig/schdist_1.1.png", 1200, 1600)
plot(linelist, col = "#00000010")
dev.off()
