library(tidyverse)
library(lubridate)

ll <- data.table::fread("../flovid19-data/linelist_latest.csv")

ll$EventDate <- ymd_hms(ll$EventDate) %>% as.Date
ll$ChartDate <- ymd_hms(ll$ChartDate) %>% as.Date
# ll$EventDate <- ymd_hms(ll$EventDate)
# ll$ChartDate <- ymd_hms(ll$ChartDate)
ll$ret_day <- as.numeric(ll$ChartDate - ll$EventDate)

#### Empirical (alpha = shape, beta = scale)
dt <- seq(ymd("2020-03-08"), ymd("2020-12-01"), by = 1)
alpha <- beta <- rep(NA, length(dt))
expect <- variance <- medians <- rep(NA, length(dt))
lqi <- uqi <- rep(NA, length(dt))
for (i in 1:length(dt)) {
  ll1 <- ll %>%
    filter(EventDate == dt[i]) %>%
    filter(ret_day >= 0, ret_day <= 45)
  
  expect[i] <- mean(ll1$ret_day)
  medians[i] <- median(ll1$ret_day)
  lqi[i] <- quantile(ll1$ret_day, 0.25)
  uqi[i] <- quantile(ll1$ret_day, 0.75)
  variance[i] <- var(ll1$ret_day)
  alpha[i] <- expect[i]^2 / variance[i]
  beta[i] <- variance[i] / expect[i]
}

par(mfrow=c(2,2))
plot(dt, alpha, type = 'l')
plot(dt, expect, type = 'l', ylab = "Expectation")
plot(dt, beta, type = 'l')
plot(dt, sqrt(variance), type = 'l', ylab = "Standard Deviation")

ggplot(data.frame(dt=dt, expect=medians, lqi = lqi, uqi = uqi)) +
  geom_line(aes(x=dt, y=expect), lwd = 1.0) +
  geom_ribbon(aes(x=dt, ymin = lqi, ymax = uqi), alpha = 0.3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_log10() +
  labs(x = "", y = "Days", title = "Case reporting delay") +
  ggpubr::theme_pubclean(base_size = 9) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey"))
ggsave("fig/case_report_delay.png", width = 16, height = 8, units = "cm")

#### Smooth using splines
library(mgcv)
day <- 1:length(dt)
mod_expect_nolim <- gam(log(expect) ~ s(day))

dt_extend <- ymd("2020-03-01") + ddays(1:184 - 1)
newdf <- data.frame(day = 1:184)

par(mfrow=c(1,1))
plot(dt_extend, exp(predict(mod_expect_nolim, newdf)), type="l",
     xlab = "", ylab = "Expectation", main = "Assym. at 0")
points(dt, expect)

mod_expect_lim <- gam(log(expect-0.2) ~ s(day))
plot(dt_extend, exp(predict(mod_expect_lim, newdf)) + 0.2, type="l",
     ylim = c(0, max(exp(predict(mod_expect_lim, newdf)) + 0.2)),
     xlab = "", ylab = "Expectation", main = "Assym. at 0.2")
points(dt, expect)

sd1 <- sqrt(variance)
mod_sd_nolim <- gam(log(sd1) ~ s(day))
plot(dt_extend, exp(predict(mod_sd_nolim, newdf)), type="l",
     ylim = c(0, max(exp(predict(mod_sd_nolim, newdf)))),
     xlab = "", ylab = "SD", main = "Assym. at 0")
points(dt, sd1)

mod_sd_lim <- gam(log(sd1-0.5) ~ s(day))
plot(dt_extend, exp(predict(mod_sd_lim, newdf)) + 0.5, type="l",
     ylim = c(0, max(exp(predict(mod_sd_lim, newdf)) + 0.5)),
     xlab = "", ylab = "SD", main = "Assym. at 2")
points(dt, sd1)

mod_expect <- exp(predict(mod_expect_lim, newdf)) + 0.2
mod_sd <- exp(predict(mod_sd_lim, newdf)) + 0.5
mod_variance <- mod_sd^2

mod_alpha <- mod_expect^2 / mod_variance
mod_beta <- mod_variance / mod_expect
plot(dt_extend, mod_alpha, type="l")
plot(dt_extend, mod_beta, type="l")

df <- data.frame(date = dt_extend,
                 mean = mod_expect,
                 sd = mod_sd,
                 a_shape = mod_alpha,
                 b_scale = mod_beta)

write_csv(df, "ts/case_report_delay.csv")
