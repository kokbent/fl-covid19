#### BRFSS
rm(list = ls())

library(raster)
library(sf)
library(data.table)
library(tidyverse)

brfss <- fread("data/BRFSS2016_SUBSET.csv")
# cnt <- st_read("tmp/cntdem/cntdem_acs_2018.shp")

#### Recode BRFSS for COVID usage
brfss_covid <- brfss %>%
  select(`_LLCPWT`, CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, CHCCOPD1, DIABETE3,
         `_BMI5CAT`, AGE, SEX, `_IMPCTY`)

## Remove no age or no sex
brfss_covid <- brfss_covid %>%
  filter(AGE >= 18 & !is.na(SEX))

## Reclassify conditions to boolean
classify <- function (v) {
  case_when(
    v == 1 ~ 1,
    v <= 4 ~ 0,
    T ~ NA_real_
  )
}

classify_bmi <- function (v) {
  case_when(
    v == 4 ~ 1,
    v < 4 ~ 0,
    T ~ NA_real_
  )
}

cond1 <- c("CVDINFR4", "CVDCRHD4", "CVDSTRK3", "ASTHMA3", "CHCCOPD1", 
            "DIABETE3")
brfss_covid[,cond1] <- classify(brfss_covid[,cond1]) %>% as.logical
brfss_covid$`_BMI5CAT` <- classify_bmi(brfss_covid$`_BMI5CAT`) %>% as.logical
brfss_covid$UNDLYCOND <- apply(brfss_covid[,c(cond1, "_BMI5CAT")], 1, any)

## Reclassify Age
brfss_covid$AGEGRP <- cut(brfss_covid$AGE, 
                          breaks = c(18, 30, 40, 50, 60, 70, 80, 100), 
                          right = F)

## Some quick analysis
table(brfss_covid$AGE, useNA = "ifany")
table(brfss_covid$SEX, useNA = "ifany")
table(brfss_covid$AGEGRP, brfss_covid$`_IMPCTY`, useNA = "ifany")
table(brfss_covid$AGEGRP, brfss_covid$UNDLYCOND, useNA = "ifany") %>% prop.table(1)

## Rename the underscore
brfss_covid <- brfss_covid %>%
  rename(IMPCTY = `_IMPCTY`,
         LLCPWT = `_LLCPWT`)
brfss_covid$LLCPWT1 <- brfss_covid$LLCPWT * nrow(brfss_covid) / sum(brfss_covid$LLCPWT)

#### Model?
library(brms)
mod <- brm(UNDLYCOND | weights(LLCPWT1) ~ SEX + AGEGRP + (1|IMPCTY),
           data = brfss_covid,
           family = bernoulli(link = 'logit'),
           warmup = 1000,
           iter = 2000,
           chains = 2,
           cores = 2,
           seed = 123)
mod <- add_criterion(mod, "waic")

newdf <- expand.grid(SEX = 1:2,
                     AGEGRP = levels(brfss_covid$AGEGRP),
                     IMPCTY = unique(brfss_covid$IMPCTY))

brfss_probs <- cbind(newdf, prob = predict(mod, newdf)[,'Estimate'])

fwrite(brfss_probs, "data/brfss_probs.csv")
