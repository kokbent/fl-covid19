# untar("sim_pop-florida-1.01.tgz", exdir = ".")

library(tidyverse)
library(leaflet)
library(geosphere)
library(RSQLite)

con <- DBI::dbConnect(RSQLite::SQLite(), "sim_pop-florida-1.1/sim_pop-florida-1.1.sqlite")

sql <- "SELECT locid FROM loc WHERE type = 'h'"

hh_locid <- dbGetQuery(con, sql)
hh_locid <- hh_locid$locid

#### Randomly select one locid and extract information
rnd_hh_locid <- sample(hh_locid, 1)

sql <- paste0("SELECT * FROM loc WHERE locid = ", rnd_hh_locid)
rnd_loc <- dbGetQuery(con, sql)
rnd_hh_hid <- rnd_loc$hid

## PID OF SELECTED HH MEMBERS
sql <- paste0("SELECT * FROM pers WHERE hid = ", rnd_hh_hid)
rnd_pers <- dbGetQuery(con, sql)
rnd_pers_pid <- rnd_pers$pid

## LOCID OF EXTRACURRICULAR ACTIVITES OF ALL MEMBERS OF THE SELECTED HH
tmp1 <- paste0(rnd_pers_pid, collapse = ",")
tmp <- paste0("(", tmp1, ")")
sql <- paste0("SELECT * FROM extracurr WHERE pid IN", tmp)
rnd_extracurr <- dbGetQuery(con, sql)
rnd_extracurr <- rnd_extracurr %>%
  pivot_longer(-pid, names_to = "order", values_to = "dest_locid")
rnd_extracurr_locid <- unique(rnd_extracurr$dest_locid)

## LOCID OF HH THAT THE SELECTED HH ITERACTS WITH
sql <- paste0("SELECT * FROM hh_network WHERE locid1 = ", rnd_hh_locid, 
              " OR locid2 = ", rnd_hh_locid)
rnd_hhnetwork <- dbGetQuery(con, sql)
rnd_hhnetwork_locid <- c(rnd_hhnetwork$locid1, rnd_hhnetwork$locid2) %>%
  unique()
rnd_hhnetwork_locid <- rnd_hhnetwork_locid[rnd_hhnetwork_locid != rnd_hh_locid]

## LOCID OF DAYTIME MOVEMENT OF ALL MEMBERS OF THE SELECTED HH
sql <- paste0("SELECT locid, type FROM movement WHERE pid IN", tmp)
rnd_daytime <- dbGetQuery(con, sql)
rnd_daytime$type <- case_when(
  rnd_daytime$type == "s" ~ "Daytime School",
  rnd_daytime$type == "w" ~ "Daytime Workplace"
)

## ALL LOCATIONS ASSOCIATES WITH ALL MEMBERS OF SELECTED HH
locations <- data.frame(locid = c(rnd_hh_locid, rnd_extracurr_locid, 
                                  rnd_hhnetwork_locid, rnd_daytime$locid),
                        locationLabel = c("Selected Home",
                                          rep("Extracurricular Business", length(rnd_extracurr_locid)),
                                          rep("Connected Home", length(rnd_hhnetwork_locid)),
                                          rnd_daytime$type))

## GET XY OF ALL LOCATIONS
tmp1 <- paste0(locations$locid, collapse = ",")
tmp <- paste0("(", tmp1, ")")
sql <- paste0("SELECT locid, x, y, type FROM loc WHERE locid IN", tmp)
loc_xy <- dbGetQuery(con, sql)
locations <- locations %>%
  left_join(loc_xy)

## COUNT TYPES OF CONNECTIONS
numConnectHome = (locations %>% filter(locationLabel == "Connected Home") %>% tally)[1,1]
numExBiz = (locations %>% filter(locationLabel == "Extracurricular Business") %>% tally)[1,1]
numDayBiz = (locations %>% filter(locationLabel == "Daytime Workplace") %>% tally)[1,1]
numDaySchool = (locations %>% filter(locationLabel == "Daytime School") %>% tally)[1,1]


#MAPPING COLOR SCHEME
pal = colorFactor(c('#ff0d00', '#3db400', '#0060ff', '#b87000', '#e300ff'), 
                  domain = c("Connected Home", "Daytime School", "Daytime Workplace", "Extracurricular Business", "Selected Home"))

#CREATE DF FOR DRAWING LINES AND CALCULATING DISTANCES (IN M AND MI)
locLines = data.frame()
home_x <- locations$x[1]
home_y <- locations$y[1]
for(i in 2:nrow(locations)){
  locid <- locations[i, "locid"]
  xy <- locations[i, c("x", "y")] %>% as.matrix()
  distanceM = distHaversine(c(home_x, home_y), xy) %>% round()
  distanceMi = (distanceM/1609) %>% round(digits = 3)
  
  df <- data.frame(locid = c(NA, locid),
                   x = c(home_x, xy[1]),
                   y = c(home_y, xy[2]),
                   distanceM = c(NA, distanceM), 
                   distanceMi = c(NA, distanceMi))
  
  locLines = rbind(locLines, df)
}

#APPEND LOCATION DF WITH DISTANCES
locations = left_join(locations, locLines)

#INTERACTIVE MAPPING
#CLICK ON CIRLCES FOR MORE INFORMATION ABOUT THAT LOCATION(LOCATION ID, TYPE, DISTANCE TO SELECTED HOME)
leaflet(data = locations) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~x, lat = ~y, color = ~pal(locationLabel), 
                   popup = ~as.character(paste(paste0(locid, ", ", locationLabel, ", location type ", type), 
                                               paste0(locid, " is ", distanceM, " m or ", 
                                                      distanceMi, " mi away from ", rnd_hh_locid),
                                               sep = "<br/>")),
                   label = ~as.character(paste0(locid, ", ", locationLabel, ", location type ", type))) %>% 
  addPolylines(lng = ~x, lat = ~y, data = locLines,
               color = "black", weight = 2) %>%
  addScaleBar(position = c("bottomleft"))

#OUTPUT SOME SUMMARIZING INFORMATION ABOUT THE SELECTED HOME AND GENERATED PLOT
print(paste0("The total number of locations associate with home ", rnd_hh_locid, " is ", (nrow(locations)-1), " and ", 
             ifelse(nrow(rnd_pers) > 1, paste0(nrow(rnd_pers), " people live"), paste0(nrow(rnd_pers), " person lives")), 
             " at this home (purple circle)."))
print(paste0("There are ", numConnectHome, " connected homes (red circles), ", numExBiz, " extracurricular businesses (tan circles), ", 
             numDayBiz, " daytime workplaces (blue circles), and ", numDaySchool, " daytime schools (green circles)."))
