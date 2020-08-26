rm(list = ls())

#### Assign underlying health conditions
library(data.table)
library(tidyverse)
source("state-sim/code/data_path.R")

#### Load Data ----
brfss <- fread(p2_brfssprobs)
pers <- fread("state-sim/output/pers_w_wid.csv")
hh <- fread("state-sim/output/hh_coords.csv")

#### Assign "number of visitors" (as weight) to all WP, NH and HF
set.seed(4326 + 23)
pers1 <- pers %>%
  select("PID", "HID", "SEX", "AGE") %>%
  left_join(hh %>% select("HID", "COUNTYFP10"))
pers1$AGEGRP <- cut(pers1$AGE, 
                    breaks = c(18, 30, 40, 50, 60, 70, 80, 100), 
                    right = F)

#### For loops through county, age to sample underlying conditions
cty <- unique(pers1$COUNTYFP10)
ageg <- levels(pers1$AGEGRP)
pers1$UNDLYCOND <- NA

for (i in 1:length(cty)) {
  
  for (j in 1:length(ageg)) {
    prob_sex1 <- brfss %>%
      filter(AGEGRP == ageg[j] & IMPCTY == cty[i] & SEX == 1) %>%
      .$prob
    prob_sex2 <- brfss %>%
      filter(AGEGRP == ageg[j] & IMPCTY == cty[i] & SEX == 2) %>%
      .$prob
    
    cond1 <- with(pers1, AGEGRP == ageg[j] & COUNTYFP10 == cty[i] & SEX == 1)
    cond2 <- with(pers1, AGEGRP == ageg[j] & COUNTYFP10 == cty[i] & SEX == 2)
    cond1 <- ifelse(is.na(cond1), F, cond1)
    cond2 <- ifelse(is.na(cond2), F, cond2)
    
    pers1$UNDLYCOND[cond1] <- rbinom(sum(cond1), 1, prob_sex1)
    pers1$UNDLYCOND[cond2] <- rbinom(sum(cond2), 1, prob_sex2)
  }
  
}

## Under 18
cond <- is.na(pers1$UNDLYCOND) & pers1$AGE <= 5
pers1$UNDLYCOND[cond] <- rbinom(sum(cond), 1, 0.139)

cond <- is.na(pers1$UNDLYCOND) & pers1$AGE > 5 & pers1$AGE <= 11
pers1$UNDLYCOND[cond] <- rbinom(sum(cond), 1, 0.184)

cond <- is.na(pers1$UNDLYCOND) & pers1$AGE > 11 & pers1$AGE <= 17
pers1$UNDLYCOND[cond] <- rbinom(sum(cond), 1, 0.206)

#### Merge and Export
pers <- pers %>%
  left_join(pers1 %>% select(PID, UNDLYCOND))

fwrite(pers, "state-sim/output/pers_w_wid.csv")
