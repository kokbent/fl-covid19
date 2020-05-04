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
sql <- "SELECT pid, sex, age FROM pers"
pers <- dbGetQuery(con, sql)
message("Preview for pers")
print(head(pers))
write_delim(pers, "pers.txt")

## Movement
sql <- "SELECT * FROM movement"
movement <- dbGetQuery(con, sql)
message("Preview for movement")
print(head(movement))
write_delim(movement, "movement.txt")

## Reside
sql <- "SELECT * FROM reside"
reside <- dbGetQuery(con, sql)
message("Preview for reside")
print(head(reside))
write_delim(reside, "reside.txt")

#### Finish and kill the unpacked files
message("Removing unpacked files...")
dbDisconnect(con)
unlink("./gen_dat", recursive = T)
message("Done.")
