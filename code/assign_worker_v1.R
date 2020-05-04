# Test run of allocating workplaces
rm(list=ls())

library(Rcpp)
library(tidyverse)
library(cvTools)
library(future)
library(doFuture)


sourceCpp("code/gravity.cpp")

wp <- read_csv("output/wp.csv")
pers <- read_csv("output/person_details.csv")
hh <- read_csv("output/hh_coords.csv")

head(wp)
head(pers)

emp_pers <- pers %>%
  filter(EMPSTATD >= 10 & EMPSTATD < 14) %>%
  filter(SCHOOL == 1)

emp_pers <- emp_pers %>% left_join(hh %>% select(HID, x, y))

rm(hh)

wp_mat <- wp[,c("x", "y")] %>% as.matrix
weights <- wp$WORKER
pts_mat <- emp_pers[,c("x", "y")] %>% as.matrix

# Split wp_mat into five
tmp <- cvFolds(nrow(pts_mat))
k <- rep(NA, nrow(pts_mat))
k[tmp$subsets] <- tmp$which
rm(tmp)

pts_mat_list <- list()
for (i in 1:5) {pts_mat_list[[i]] <- pts_mat[k==i,]}
pts_mat_list %>%
  map_int(~ nrow(.x))

seeds <- 4326:4330
# assign_mat_list <- pts_mat_list %>%
#   future_map(~ assign_by_gravity(.x, wp_mat, weights, 1000, 4326, steps = 15))


plan(multiprocess(workers = 5))
registerDoFuture()
assignment <- foreach(i=1:5, .noexport = "assign_by_gravity",
                      .packages = "Rcpp") %dopar% {
  sourceCpp("code/gravity.cpp")
  assign_by_gravity(pts_mat_list[[i]],
                    wp_mat,
                    weights,
                    1000,
                    seeds[i],
                    steps = 15)
}
plan(sequential)

assignment2 <- assignment
wp_assignment <- data.frame()
for (i in 1:5) {
  ord <- order(assignment2[[i]][,1])
  assignment2[[i]] <- assignment2[[i]][ord,]
  
  pid <- emp_pers[k==i, "PID"]
  wid <- wp[assignment2[[i]][,2], "WID"]
  
  df <- cbind(pid, wid)
  wp_assignment <- rbind(wp_assignment, df)
}

tmp <- wp_assignment %>% group_by(WID) %>% count()
wp2 <- wp %>%
  left_join(tmp) %>%
  mutate(n = ifelse(is.na(n), 0, n))
plot(wp2$WORKER, wp2$n, pch = ".")

write_csv(wp_assignment, "./tmp/wp_assignment.csv")
