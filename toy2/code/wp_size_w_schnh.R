rm(list=ls())

#### Model sizes of workplaces based on WAC info and NAICS employment sizes
library(raster)
library(tidyverse)
library(data.table)
library(Rcpp)

source("toy2/code/data_path.R")
source("toy2/code/target_func.R")
sourceCpp("code/action_by_gravity.cpp")

#### Loading data ----
## cenacs
cenacs <- shapefile(p2_cenacs)

## lehdwac
wac <- shapefile(p2_wac)
wac <- wac@data

## ncd
wp_coords <- fread(p2_ncdwp)
wp_coords <- wp_coords %>%
  select(-X1, -naics_classification_notes)

## NH
if (file.exists("toy2/output/nh.csv")) {
  nh <- fread("toy2/output/nh.csv") %>%
    mutate(SERIAL = NHID, TYPE = "n") %>%
    select(SERIAL, x, y, WORKER, TYPE)
} else {
  stop("NH data not found.")
}

## SCH
if (file.exists("toy2/output/sch.csv")) {
  sch <- fread("toy2/output/sch.csv") %>%
    mutate(SERIAL = SID, TYPE = "s") %>%
    select(SERIAL, x, y, WORKER, TYPE)
} else {
  stop("SCH data not found.")
}

## HF
if (file.exists("toy2/output/hf.csv")) {
  hf <- fread("toy2/output/hf.csv") %>%
    mutate(SERIAL = HFID, TYPE = "hf") %>%
    select(SERIAL, x = X, y = Y, WORKER, TYPE)
} else {
  stop("HF data not found.")
}


## NAICS
if (file.exists("toy2/data/naics_emp_wpar.csv")) {
  naics <- fread("toy2/data/naics_emp_wpar.csv")
  colnames(naics) <- c("NAICS 1 Code", "Description", paste0("grp", 1:9), "s", "xi")
} else {
  stop("NAICS params data not found.")
}

## NAICS 2017 - 2012 Lookup (Needed to reconcile NAICS differences)
naics_1712 <- fread("toy2/data/naics_1712_lookup.csv")

#### New dataframe ----
## Jobs per census tract
jobs_tract <- wac %>%
  group_by(TRACTCE10) %>%
  summarise(JOBS = sum(TOTAL_JOBS))
cenacs$TRACTCE10 %>% unique %>% length

## WP without SCH and NH 
wp_coords1 <- wp_coords %>%
  dplyr::filter(!is.na(x), !is.na(y)) %>%
  dplyr::filter(!(naics >= 611110 & naics <= 611399) | is.na(naics)) %>%
  dplyr::filter(!naics %in% c(623312, 623110) | is.na(naics)) %>%
  dplyr::filter(!(naics >= 622100 & naics <= 622399) | is.na(naics))
wp_coords1$WORKER <- NA
wp_coords1 <- wp_coords1 %>%
  select(SERIAL = serial, x, y, WORKER, NAICS = naics, naics_essential_classification) %>%
  mutate(TYPE = "w")

## Reconcile NCD's hospitals and AHCA HF
wp_coords_hf <- wp_coords %>%
  dplyr::filter((naics >= 622100 & naics <= 622399))
hf1 <- hf %>%
  arrange(desc(WORKER))

# Find nearest NCD's hospitals to AHCA HF and remove
for (i in 1:nrow(hf1)) {
  hf_xy <- as.matrix(hf1[i,c("x", "y")])
  wp_hf_xymat <- as.matrix(wp_coords_hf[,c("x", "y")])
  
  tmp <- assign_by_gravity(pts = hf_xy, 
                           locs = wp_hf_xymat, 
                           weights = rep(1, nrow(wp_hf_xymat)),
                           num_loc = 1, 
                           seed = i)
  wp_coords_hf <- wp_coords_hf[-tmp[,2],]
}

wp_coords_hf$WORKER <- NA
wp_coords_hf <- wp_coords_hf %>%
  select(SERIAL = serial, x, y, WORKER, NAICS = naics, naics_essential_classification) %>%
  mutate(TYPE = "w")

# Add Reconciled hospitals, SCH, NH and HF
wp_coords2 <- bind_rows(hf, wp_coords1, wp_coords_hf, nh, sch)


#### Extra analyses ----
## WP with same coordinates
wp_unique <- wp_coords %>%
  dplyr::filter(!is.na(x), !is.na(y)) %>%
  group_by(x, y, naics) %>%
  count() %>%
  arrange(desc(n))

naics_unique <- table(wp_unique$naics, wp_unique$n > 1)
tmp <- rownames(naics_unique) %>% as.numeric
ind <- which(tmp >= 611110 & tmp <= 611699) # Schools
naics_unique[ind,]

#### Meat ----
## Sort WP into census tracts
wp_sp <- SpatialPoints(wp_coords2[,c("x", "y")], proj4string = CRS("+init=epsg:4326"))
wp_sp <- wp_sp %>%
  spTransform(crs(cenacs))

cenacs2 <- cenacs
cenacs2@data <- cenacs@data[,c("TRACTCE10", "BLKGRPCE10")]
tmp <- over(wp_sp, cenacs2)
wp <- cbind(wp_coords2, tmp)

rm(cenacs2)
rm(tmp)

wp <- wp %>%
  filter(!is.na(TRACTCE10)) # Get rid of wp outside FL

## Clean up discrepancies in NAICS between tables
wp1 <- wp %>%
  select(-BLKGRPCE10) %>%
  mutate(NAICS = ifelse(is.na(NAICS) | NAICS == 0, 999999, NAICS))

naics_nomatch <- wp1$NAICS[!wp1$NAICS %in% naics$`NAICS 1 Code`] %>%
  unique

tmp <- sapply(naics_nomatch, 
              function(x) min(naics_1712$NAICS_2017[naics_1712$NAICS_2012 == x]))

naics_lookup <- cbind(naics_nomatch, tmp)

# Some entries need hard coding to reconcile :/
naics_lookup[naics_lookup[,1] == 425110, 2] <- 425120
naics_lookup[naics_lookup[,1] == 485991, 2] <- 485999
naics_lookup[naics_lookup[,1] == 541711, 2] <- 541714
naics_lookup[naics_lookup[,1] == 541712, 2] <- 541714
naics_lookup[naics_lookup[,1] == 813311, 2] <- 813312

cond <- wp1$NAICS %in% naics_lookup[,1]
ind <- sapply(wp1$NAICS[cond], function(x) which(x == naics_lookup[,1]))
wp1$NAICS[cond] <- naics_lookup[ind, 2]
table(wp1$NAICS[!wp1$NAICS %in% naics$`NAICS 1 Code`]) # double check

## Join WP with naics data
# wp1 <- wp1 %>%
#   left_join(naics %>% rename(NAICS = `NAICS 1 Code`) %>% select(-Description))

# SERIAL is not UNIQUE (Mix of SID, NHID and original UID for WP)
wp1$WID <- 1:nrow(wp1) 

## JOBS and JOBS to be filled for each census tract
nwp_jobs <- wp1 %>%
  group_by(TRACTCE10) %>%
  summarise(WP_TOFILL = sum(is.na(WORKER)), 
            CURR_JOBS = sum(WORKER, na.rm = T)) %>% 
  left_join(jobs_tract) %>%
  mutate(JOBS_TOFILL = JOBS - CURR_JOBS,
         JOBS_TOFILL = ifelse(JOBS_TOFILL<0, 0, JOBS_TOFILL))

## Filling up jobs
jobs_mat <- c()
pb <- txtProgressBar(max = nrow(nwp_jobs), style = 3)

for (i in 1:nrow(nwp_jobs)) {
  setTxtProgressBar(pb, i)
  wp_tract <- wp1 %>%
    filter(TRACTCE10 == nwp_jobs$TRACTCE10[i], is.na(WORKER)) %>%
    arrange(NAICS) %>%
    select(WID, WORKER, NAICS)
  wp_tract_agg <- wp_tract %>%
    group_by(NAICS) %>%
    count %>%
    left_join(naics %>% rename(NAICS = `NAICS 1 Code`) %>% select(-Description),
              by = "NAICS")
  
  if (nwp_jobs$JOBS_TOFILL[i] <= nwp_jobs$WP_TOFILL[i]) {
    wp_tract$WORKER <- 1
  } else {
    grpmat <- wp_tract_agg[,colnames(wp_tract_agg) %>% str_detect("grp")] %>%
      as.matrix
    n <- wp_tract_agg$n
    s <- wp_tract_agg$s
    xi <- wp_tract_agg$xi
    
    rmat <- pmap(list(n = n, ind = 1:nrow(grpmat), s = s, xi = xi),
                 function (n, ind, s, xi) two_stage_samp(n, grpmat[ind,], s, xi))
    rmat <- do.call("rbind", rmat)
    
    # Create 100 sets random gpd numbers
    colSums(rmat)
    nwp_jobs$JOBS_TOFILL[i]
    
    # Mean of generalized pareto is unstable, choosing sample set with 
    # sum of sizes closest to JOBS_TO_FILL
    sel <- which.min(abs(colSums(rmat) - nwp_jobs$JOBS_TOFILL[i]))
    r <- rmat[,sel]
    
    # Scale to ideal mean
    diff <- nwp_jobs$JOBS_TOFILL[i] - length(r)
    ratio <- diff / (sum(r) - length(r))
    r2 <- r - 1
    r2 <- 1 + round(r2 * ratio)
    
    wp_tract$WORKER <- r2
  }
  
  w <- wp_tract[,c("WID", "WORKER")] %>% as.matrix
  jobs_mat <- rbind(jobs_mat, w)
}

jobs_mat1 <- jobs_mat[order(jobs_mat[,1]),]
wp1$WORKER[is.na(wp1$WORKER)] <- jobs_mat1[,2]

## Verify that we create enough workers
foo <- wp1 %>%
  group_by(TRACTCE10) %>%
  summarise(WORKER = sum(WORKER))
plot(log10(foo$WORKER), log10(nwp_jobs$JOBS))
abline(0, 1)


#### Export ----
wp2 <- wp1 %>%
  select(WID, TYPE, x, y, WORKER, SERIAL, NAICS, ESS_CLASS = naics_essential_classification)

fwrite(wp2, "toy2/output/wp.csv")
