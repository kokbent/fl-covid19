library(data.table)
library(tidyverse)
library(lubridate)
library(googlesheets4)

dat <- fread("https://data.cdc.gov/api/views/xkkf-xrst/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=")
dat1 <- dat %>%
  filter(State == "Florida", Type == "Predicted (weighted)", Outcome == "All causes") %>%
  mutate(`Week Ending Date` = ymd(`Week Ending Date`),
         Week = epiweek(`Week Ending Date`))
dat2 <- dat1 %>%
  filter(year(`Week Ending Date`) == 2020, Week >= 10, Week <= 42) %>%
  mutate(excess_by_threshold = `Excess Lower Estimate`,
         excess_by_expect = `Excess Higher Estimate`)

death_by_dd <- data.table::fread("../fl-covid19-extra-analysis/data/death_day_20201110.csv")
death_by_dd <- death_by_dd %>%
  mutate(Week = epiweek(as.Date(Date))) %>%
  group_by(Week) %>%
  summarise(rdeath = sum(Deaths))

dat3 <- dat2 %>%
  left_join(death_by_dd)

ggplot(dat3) +
  # geom_line(aes(`Week Ending Date`, `Excess Lower Estimate`), lty = 2) +
  geom_line(aes(`Week Ending Date`, `Excess Higher Estimate`), lty = 2) +
  geom_line(aes(`Week Ending Date`, rdeath), lty = 1) +
  labs(y = "Death",
       subtitle = "Solid line = Reported COVID Death; Dashed line = estimate of excess death")

dat_out <- dat3 %>%
  select(week = Week, excess_by_threshold, excess_by_expect, rdeath)
fwrite(dat_out, "ts/excess-florida.csv")
