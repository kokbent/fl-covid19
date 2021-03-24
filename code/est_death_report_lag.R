library(tidyverse)
library(lubridate)

report_dates <- seq(ymd("2020-04-22"), ymd("2021-01-20"), by = 1)

df <- data.frame()
for (i in 1:length(report_dates)) {
  f <- paste0("../florida-covid19-deaths-by-day/deathsbydateexport_",
              str_remove_all(report_dates[i], "-"),
              ".csv")
  if (!file.exists(f)) next
  dat <- read_csv(f)
  dat$Date <- as.POSIXct(dat$Date / 1000, origin = "1970-01-01")
  
  dat$Report_Date <- report_dates[i]
  df <- bind_rows(df, dat)
}

df$Date <- as.Date(df$Date)
df$lag <- df$Report_Date - df$Date
df1 <- df %>%
  filter(lag <= 49, Date >= ymd("2020-04-23"))
df1$lag <- as.numeric(df1$lag)

df2 <- df1 %>%
  group_by(Date) %>%
  mutate(maxlag = max(lag)) %>%
  filter(maxlag >= 40)

df2 %<>% 
  arrange(Date, Report_Date) %>%
  group_by(Date) %>%
  mutate(Incd = diff(c(0, Deaths))) %>%
  mutate(Incd = ifelse(Incd < 0, 0, Incd))

df2 %>%
  summarise(day = sum(Incd * lag) / sum(Incd),
            med = median(rep(lag, Incd)),
            lqi = quantile(rep(lag, Incd), 0.25),
            uqi = quantile(rep(lag, Incd), 0.75)) %>%
  ggplot() +
  geom_line(aes(x=Date, y=med)) +
  geom_smooth(aes(x=Date, y=med), se=F) +
  # geom_line(aes(x=ChartDate, y=day), col = "red") +
  geom_ribbon(aes(x=Date, ymin=lqi, ymax=uqi), alpha=0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Days between death and death report",
       subtitle = "Median and IQR", x="Date of death", y="Day")
ggsave("fig/death-report-delay.png", width = 16, height = 8)


v <- rep(df2$lag, df2$Incd)
negloglik <- function(pars, v) {
  ll <- dnbinom(v, pars[1], pars[2], log = T)
  return(-sum(ll))
}

optim(c(5, 0.2), fn = negloglik, lower = c(0, 0), upper = c(Inf, 1), v = v)