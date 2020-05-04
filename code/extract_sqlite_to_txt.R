rm(list=ls())

if (!require("dplyr")) stop("dplyr package is required but not installed.")
if (!require("readr")) stop("readr package is required but not installed.")
if (!require("RSQLite")) stop("RSQLite package is required but not installed.")

#### Unpack
message("Unpacking files...")
if (!file.exists("gen_dat.tgz")) stop("File gen_dat.tgz must be in the same folder as this script.")
untar("gen_dat.tgz", exdir=".")

#### Connect
con <- dbConnect(RSQLite::SQLite(), "gen_dat/gen_dat.sqlite")

#### Pulling query using SQL
## Locations
sql <- "SELECT locid, x, y, type FROM loc"
loc <- dbGetQuery(con, sql)

sql <- "SELECT locid, essential FROM wp"
wp <- dbGetQuery(con, sql)

loc <- loc %>%
  left_join(wp)
loc$essential[loc$type == "s"] <- "n"
loc$essential[loc$type == "n"] <- "y"

message("Preview for loc")
print(head(loc))
write_delim(loc, "loc.txt")

## Persons
sql <- "SELECT p.pid AS pid, r.locid AS res_id, p.sex AS sex, p.age AS age, m.locid AS mov_id
FROM pers AS p
LEFT JOIN movement AS m
ON p.pid = m.pid
LEFT JOIN reside AS r
ON p.pid = r.pid"
pers <- dbGetQuery(con, sql)
message("Preview for pers")
print(head(pers))
write_delim(pers, "pers.txt")

#### Finish and kill the unpacked files
message("Removing unpacked files...")
dbDisconnect(con)
unlink("./gen_dat", recursive = T)
message("Done.")
