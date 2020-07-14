rm(list=ls())

if (!require("dplyr")) stop("dplyr package is required but not installed.")
if (!require("readr")) stop("readr package is required but not installed.")
if (!require("RSQLite")) stop("RSQLite package is required but not installed.")

#### Unpack
message("Unpacking files...")
if (!file.exists("sim_pop-florida.tgz")) stop("File sim_pop-florida.tgz must be in the same folder as this script.")
untar("sim_pop-florida.tgz", exdir=".")

#### Connect
con <- dbConnect(RSQLite::SQLite(), "sim_pop-florida/sim_pop-florida.sqlite")

#### Pulling query using SQL
## Locations
sql <- "SELECT locid, x, y, type FROM loc"
loc <- dbGetQuery(con, sql)

sql <- "SELECT locid, essential FROM wp"
wp <- dbGetQuery(con, sql)

loc <- loc %>%
  left_join(wp)
colnames(loc) <- c("locid", "x", "y", "type", "essential")
loc$essential[loc$type == "h"] <- "y"
loc$essential[loc$type == "s"] <- "y"
loc$essential[loc$type == "n"] <- "y"
loc$locid <- loc$locid - 1
loc$x <- round(loc$x, 5)
loc$y <- round(loc$y, 5)

message("Preview for loc")
print(head(loc))
write_delim(loc, "locations-florida.txt")

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
write_delim(pers, "population-florida.txt")

## Simply copying the network files out
file.copy("sim_pop-florida/network-florida.txt", ".")

#### Finish and kill the unpacked files
message("Removing unpacked files...")
dbDisconnect(con)
unlink("./sim_pop-florida", recursive = T)
message("Done.")
