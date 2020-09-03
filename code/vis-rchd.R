library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)

line_list <- fread("../flovid19-data/linelist_latest.csv")

line_list$EventDate <- ymd_hms(line_list$EventDate) %>% as.Date()
line_list$ChartDate <- ymd_hms(line_list$ChartDate) %>% as.Date()

ll <- line_list
ll$Week <- epiweek(ll$EventDate)
ll$Age_group <- as.character(ll$Age_group)
ll$Age_group[ll$Age_group == "5-14 years"] <- "05-14 years"
ll$Age_group[ll$Age_group == "0-4 years"] <- "00-04 years"
ll <- ll %>% filter(Week >= 10, Age_group != "Unknown")
weeks <- unique(ll$Week) %>% sort
weeks_date <- rep(ymd("2020-01-04"), length(weeks))
week(weeks_date) <- weeks
age_grp <- unique(ll$Age_group)

ll_case <- ll %>%
  group_by(Week) %>%
  summarise(median_age = median(Age),
            mean_age = mean(Age))

ll_hosp <- ll %>%
  filter(Hospitalized == "YES") %>%
  group_by(Week) %>%
  summarise(median_age = median(Age),
            mean_age = mean(Age))

ll_died <- ll %>%
  filter(Died == "Yes") %>%
  group_by(Week) %>%
  summarise(median_age = median(Age),
            mean_age = mean(Age))

png("ts/rchd-florida.png", width = 7.57, height = 11, units = "in",
    res = 200, pointsize = 15)
par(mfrow=c(3, 1), mar = c(4, 4, 2, 1) + 0.1)
with(ll_case, {
  plot(Week, median_age, type = "l",
       ylim = range(c(median_age, mean_age)),
       xlab = "", ylab = "Age", main = "Reported Case")
  lines(Week, mean_age, col = "red")
  legend("topright", legend = c("median", "mean"), col = c("black", "red"), lty = 1)
})

with(ll_hosp, {
  plot(Week, median_age, type = "l",
       ylim = range(c(median_age, mean_age)),
       xlab = "", ylab = "Age", main = "Reported Hospitalization")
  lines(Week, mean_age, col = "red")
})

with(ll_died, {
  plot(Week, median_age, type = "l",
       ylim = range(c(median_age, mean_age)),
       xlab = "Lab Event Week", ylab = "Age", main = "Reported Death")
  lines(Week, mean_age, col = "red")
})
dev.off()

fwrite(ll_case, "ts/age_rcase-florida.csv")
fwrite(ll_hosp, "ts/age_rhosp-florida.csv")
fwrite(ll_died, "ts/age_rdeath-florida.csv")
