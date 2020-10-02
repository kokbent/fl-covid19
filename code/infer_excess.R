library(data.table)
library(tidyverse)
library(lubridate)

dat <- fread("https://data.cdc.gov/api/views/xkkf-xrst/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=")
dat1 <- dat %>%
  filter(State == "Florida", Type == "Unweighted") %>%
  mutate(`Week Ending Date` = ymd(`Week Ending Date`),
         Week = epiweek(`Week Ending Date`))
dat2 <- dat1 %>%
  filter(year(`Week Ending Date`) == 2020, Week >= 10, Week <= 35) %>%
  mutate(excess_by_threshold = `Excess Lower Estimate`,
         excess_by_expect = `Excess Higher Estimate`)

chd <- fread("ts/rcasedeath-florida.csv") %>%
  mutate(Week = epiweek(Date))

chd <- chd %>%
  group_by(Week) %>%
  summarise(rdeath = sum(rdeath))

dat3 <- dat2 %>%
  left_join(chd)

ggplot(dat3) +
  geom_line(aes(`Week Ending Date`, `Excess Lower Estimate`), lty = 2) +
  geom_line(aes(`Week Ending Date`, `Excess Higher Estimate`), lty = 2) +
  geom_line(aes(`Week Ending Date`, rdeath), lty = 1) +
  labs(y = "Death",
       subtitle = "Solid line = Reported COVID Death; Dashed line = Lower and upper estimate of excess death")

dat_out <- dat3 %>%
  select(week = Week, excess_by_threshold, excess_by_expect, rdeath)
fwrite(dat_out, "ts/excess-florida.csv")
