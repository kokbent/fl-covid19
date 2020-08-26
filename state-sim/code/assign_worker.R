rm(list=ls())

#### Assign workers to workplace by way of gravity
library(Rcpp)
library(tidyverse)
library(data.table)

sourceCpp("code/action_by_gravity.cpp")
wp <- fread("state-sim/output/wp.csv")
pers <- fread("state-sim/output/person_details.csv")
hh <- fread("state-sim/output/hh_coords.csv")

emp_pers <- pers %>%
  filter(EMPSTATD >= 10 & EMPSTATD < 14) %>%
  filter(SCHOOL == 1)

emp_pers <- emp_pers %>% left_join(hh %>% select(HID, x, y))

rm(hh)

## Exclude HF from aggregating because there are HF with same XY
wp2_1 <- wp %>%
  filter(TYPE == "hf") %>%
  select(x, y, TYPE, NAICS, ESS_CLASS, WORKER, SERIAL) %>%
  mutate(N = 1)
wp2_2 <- wp %>%
  filter(TYPE != "hf") %>%
  group_by(x, y, TYPE, NAICS, ESS_CLASS, SERIAL) %>%
  summarise(WORKER = sum(WORKER),
            N = n())
wp2 <- bind_rows(wp2_1, wp2_2)

wp2$WID2 <- 1:nrow(wp2)
table(wp2$TYPE)


# Scale workplace sizes up if there are more employed persons than jobs available
# Generally want to have slightly more jobs than employed persons, but in this
# toy example, we might need to scale it downwards...
wp_mat <- wp2[,c("x", "y")] %>% as.matrix
pts_mat <- emp_pers[,c("x", "y")] %>% as.matrix

ideal_workern <- nrow(pts_mat) + 1200
ratio <- 1
weights <- (wp2$WORKER * ratio) %>% round

if (sum(weights) > ideal_workern) {
  tmp <- weights
  while (sum(tmp) > ideal_workern) {
    weights <- tmp
    ratio <- ratio - 0.01
    tmp <- (wp2$WORKER * ratio) %>% round
  }
} else {
  while (sum(weights) <= ideal_workern) {
    ratio <- ratio + 0.01
    weights <- (wp2$WORKER * ratio) %>% round
  }
}


sum(weights) # This value should be substantially but not overly larger than next
nrow(pts_mat)

# Use 1000 nearest workplaces
system.time(
  assign_mat <- assign_by_gravity(pts_mat, wp_mat, 
                                  weights, 1000, 4326, steps = 1, use_capacity = T)
)

#### Check if workplaces are overassigned ----
table(assign_mat[,2]) %>% as.matrix() -> tmp
tmp1 <- weights[rownames(tmp) %>% as.numeric]
mean(tmp == tmp1)
mean(tmp > tmp1)
nrow(wp_mat) - length(tmp1) # number of workplaces with 0 workers

#### Put WID to PERS ----
ord <- order(assign_mat[,1])
assign_mat2 <- assign_mat[ord,]
head(assign_mat2)
summary(assign_mat2[,1] - 1:nrow(assign_mat)) # Double check PID being correct
emp_pers$WID2 <- assign_mat2[,2]
pers1 <- pers %>% left_join(emp_pers %>% select(PID, WID2))
table(is.na(pers1$WID2))

#### Export ----
fwrite(pers1, "state-sim/output/pers_w_wid.csv")
fwrite(wp2, "state-sim/output/wp2.csv")
