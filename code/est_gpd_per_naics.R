rm(list = ls())

library(tidyverse)
source("./code/target_func.R")

naics_emp <- read_csv("data/NAICS_employment_sizes.csv") %>%
  select(-`Uncoded records`, -`Grand Total`)
# Set NAICS of Grand total to 999999 so uncoded workplaces use
# overall params
naics_emp$`NAICS 1 Code`[nrow(naics_emp)] <- 999999 

naics_emp[,3:ncol(naics_emp)] <- apply(naics_emp[,3:ncol(naics_emp)], 2, 
                                       function (x) ifelse(is.na(x), 0, x))
naics_emp <- as.data.frame(naics_emp)

# Estimate MLE for GPD parameters
params <- matrix(NA, nrow(naics_emp), 2)
pb <- txtProgressBar(max = nrow(naics_emp), style = 3)
for (i in 1:nrow(naics_emp)) {
  setTxtProgressBar(pb, i)
  y <- naics_emp[i,3:ncol(naics_emp)] %>% as.matrix
  lgpd_target <- lgpd_target_(y)
  param <- optim(c(1, 1), lgpd_target)
  params[i,] <- param$par
}

colSums(is.na(params))
colMeans(params > 1) 

# Large portion of the sectors has no GPD without finite mean
# and even more without finite variance. Implication on drawing good
# quality numbers. Future problems to address...
naics_emp <- cbind(naics_emp, params)
colnames(naics_emp)[c(ncol(naics_emp) - 1, ncol(naics_emp))] <- c("s", "xi")

write_csv(naics_emp, "./data/naics_emp_wpar.csv")
