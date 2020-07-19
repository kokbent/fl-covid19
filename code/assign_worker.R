# Test run of allocating workplaces
rm(list=ls())

library(Rcpp)
library(tidyverse)
library(data.table)

sourceCpp("code/action_by_gravity.cpp")

wp <- fread("output/wp.csv")
pers <- fread("output/person_details.csv")
hh <- fread("output/hh_coords.csv")

head(wp)
head(pers)

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

wp_mat <- wp2[,c("x", "y")] %>% as.matrix
weights <- (wp2$WORKER * 1.05) %>% round
pts_mat <- emp_pers[,c("x", "y")] %>% as.matrix

sum(weights) # This value should be substantially but not overly larger than next
nrow(pts_mat)

system.time(
  assign_mat <- assign_by_gravity(pts_mat, wp_mat, 
                                  weights, 1000, 4326 + 4, steps = 1, use_capacity = T)
)

#### Check if workplaces are overassigned
table(assign_mat[,2]) %>% as.matrix() -> tmp
tmp1 <- weights[rownames(tmp) %>% as.numeric]
mean(tmp == tmp1)
mean(tmp > tmp1)
nrow(wp_mat) - length(tmp1) # number of workplaces with 0 workers

#### Put WID to PERS
ord <- order(assign_mat[,1])
assign_mat2 <- assign_mat[ord,]
head(assign_mat2)
summary(assign_mat2[,1] - 1:nrow(assign_mat)) # Double check PID being correct
emp_pers$WID2 <- assign_mat2[,2]
pers1 <- pers %>% left_join(emp_pers %>% select(PID, WID2))
table(is.na(pers1$WID2))

fwrite(pers1, "output/pers_w_wid.csv")
fwrite(wp2, "output/wp2.csv")
