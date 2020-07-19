rm(list=ls())

if (!require("dplyr")) stop("dplyr package is required but not installed.")
if (!require("data.table")) stop("readr package is required but not installed.")
if (!require("RSQLite")) stop("RSQLite package is required but not installed.")

#### Unpack
message("Unpacking files...")
if (!file.exists("sim_pop-florida-1.0.tgz")) stop("File sim_pop-florida-1.0.sqlite.tgz must be in the same folder as this script.")
untar("sim_pop-florida-1.0.tgz", exdir=".")

#### Connect
con <- dbConnect(RSQLite::SQLite(), "sim_pop-florida-1.0/sim_pop-florida-1.0.sqlite")

#### Pulling query using SQL
## Locations
sql <- "SELECT loc.locid, loc.x, loc.y, loc.type, nh.hf_locid FROM loc
LEFT JOIN nh
ON loc.locid = nh.locid"
loc <- dbGetQuery(con, sql)

hh_nonnh <- dbGetQuery(con, "SELECT locid, hf_locid AS hf_locid2 FROM hh WHERE hh.nh = 'n'")
loc <- loc %>%
  left_join(hh_nonnh)
loc <- loc %>%
  mutate(hf_locid = ifelse(is.na(hf_locid), hf_locid2, hf_locid)) %>%
  select(-hf_locid2)

sql <- "SELECT locid, essential FROM wp"
wp <- dbGetQuery(con, sql)

loc <- loc %>%
  left_join(wp)
colnames(loc) <- c("locid", "x", "y", "type", "hfid", "essential")
loc$essential[loc$type != "w"] <- "y"
loc$locid <- loc$locid - 1
loc$x <- round(loc$x, 5)
loc$y <- round(loc$y, 5)
loc <- loc %>%
  select(locid, x, y, type, essential, hfid) %>%
  mutate(hfid = ifelse(is.na(hfid), -1, hfid))

message("Preview for loc")
print(head(loc))
fwrite(loc, "locations-florida.txt", sep = " ")

## Persons
sql <- "SELECT p.pid AS pid, r.locid AS res_id, p.sex AS sex, p.age AS age, m.locid AS mov_id
FROM pers AS p
LEFT JOIN movement AS m
ON p.pid = m.pid
LEFT JOIN reside AS r
ON p.pid = r.pid"
pers <- dbGetQuery(con, sql)

colnames(pers) <- c("pid", "home_id", "sex", "age", "day_id")
pers$pid <- pers$pid - 1
pers$home_id <- pers$home_id - 1
pers$day_id <- pers$day_id - 1
pers$day_id[is.na(pers$day_id)] <- -1

message("Preview for pers")
print(head(pers))
fwrite(pers, "population-florida.txt", sep = " ")

rm(hh_nonnh, loc, pers, wp)

## Household network (pretty much just take from the sql db)
sql <- "SELECT * FROM hh_network"
hh_network <- dbGetQuery(con, sql)
hh_network$locid1 <- hh_network$locid1 - 1
hh_network$locid2 <- hh_network$locid2 - 1

message("Preview for hh_network")
print(head(hh_network))
fwrite(hh_network, "network-florida.txt", sep = " ", col.names = F)

rm(hh_network)
## Extracurricular activities (pretty much just take from the sql db)
sql <- "SELECT * FROM extracurr"
ec <- dbGetQuery(con, sql)
ec <- ec - 1

message("Preview for ec")
print(head(ec))
fwrite(ec, "extracurricular-florida.txt", sep = " ")

#### Finish and kill the unpacked files
message("Removing unpacked files...")
dbDisconnect(con)
unlink("./sim_pop-florida-1.0", recursive = T)
message("Done.")
