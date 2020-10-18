library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(cowplot)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

line_list <- fread("../flovid19-data/linelist_latest.csv")
model_data <- fread("../model_data_rchd")

line_list$EventDate <- ymd_hms(line_list$EventDate) %>% as.Date()
line_list$ChartDate <- ymd_hms(line_list$ChartDate) %>% as.Date()

ll <- line_list
ll$Week <- epiweek(ll$EventDate)
ll$Age <- as.numeric(ll$Age)
ll$Age_group <- as.character(ll$Age_group)
ll$Age_group[ll$Age_group == "5-14 years"] <- "05-14 years"
ll$Age_group[ll$Age_group == "0-4 years"] <- "00-04 years"
ll <- ll %>% filter(Week >= 10, Age_group != "Unknown")
weeks <- unique(ll$Week) %>% sort
weeks_date <- rep(ymd("2020-01-04"), length(weeks))
week(weeks_date) <- weeks
age_grp <- unique(ll$Age_group)

wk_to_mth <- data.frame(date = seq(ymd("2020-01-01"), max(unique(ll$EventDate) %>% sort()), by = "day"),
                        week = epiweek(seq(ymd("2020-01-01"), max(unique(ll$EventDate) %>% sort()), by = "day")),
                        month = substring(month(seq(ymd("2020-01-01"), max(unique(ll$EventDate) %>% sort()), by = "day"), label = TRUE), 1),
                        stringsAsFactors = FALSE)

convert_wk_mth <- function(w){
  getmode(wk_to_mth$month[wk_to_mth$week == w])
}

ll_case <- ll %>%
  group_by(Week) %>%
  summarise(median_age = median(Age),
            mean_age = mean(Age))

model_case <- model_data %>% 
  select(Week = week, model_median = case_median, model_mean = case_mean)

case_join <-  full_join(model_case, ll_case)
case_join$month <- ""
wk_lab <- c()
mth_lab <- c()
for(i in 1:nrow(case_join)){
  if((convert_wk_mth(case_join$Week[i]) %in% case_join$month) == FALSE){
    case_join$month[i] <- convert_wk_mth(case_join$Week[i])
    wk_lab <- c(wk_lab, case_join$Week[i])
    mth_lab <- c(mth_lab, convert_wk_mth(case_join$Week[i]))
  }
}

ll_hosp <- ll %>%
  filter(Hospitalized == "YES") %>%
  group_by(Week) %>%
  summarise(median_age = median(Age),
            mean_age = mean(Age))

model_hosp <- model_data %>% 
  select(Week = week, model_median = hosp_median, model_mean = hosp_mean)

hosp_join <- full_join(model_hosp, ll_hosp)
hosp_join$month <- ""
for(i in 1:nrow(hosp_join)){
  if((convert_wk_mth(hosp_join$Week[i]) %in% hosp_join$month) == FALSE){
    hosp_join$month[i] <- convert_wk_mth(hosp_join$Week[i])
  }
}

ll_died <- ll %>%
  filter(Died == "Yes") %>%
  group_by(Week) %>%
  summarise(median_age = median(Age),
            mean_age = mean(Age))

model_died <- model_data %>% 
  select(Week = week, model_median = death_median, model_mean = death_mean)

died_join <- full_join(model_died, ll_died)
died_join$month <- ""
for(i in 1:nrow(died_join)){
  if((convert_wk_mth(died_join$Week[i]) %in% died_join$month) == FALSE){
    died_join$month[i] <- convert_wk_mth(died_join$Week[i])
  }
}

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

case_plot <- ggplot(data = case_join) +
  geom_line(aes(x = Week, y = model_median, color = "Model Median Age", linetype = "Model Median Age")) + 
  geom_line(aes(x = Week, y = model_mean, color = "Model Mean Age", linetype = "Model Mean Age")) + 
  geom_line(aes(x = Week, y = median_age, color = "Linelist Median Age", linetype = "Linelist Median Age")) + 
  geom_line(aes(x = Week, y = mean_age, color = "Linelist Mean Age", linetype = "Linelist Mean Age")) +
  labs(title = "Cases", x = "Week", y = "Age") + 
  scale_color_manual(NULL, breaks = c("Model Median Age", "Model Mean Age", "Linelist Median Age", "Linelist Mean Age"), 
                     values = c("black", "red", "black", "red")) +
  scale_linetype_manual(NULL, breaks = c("Model Median Age", "Model Mean Age", "Linelist Median Age", "Linelist Mean Age"), 
                     values = c(2, 2, 1, 1)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.9, 0.8))

hosp_plot <- ggplot(data = hosp_join) +
  geom_line(aes(x = Week, y = model_median), linetype = 2, color = "black") + 
  geom_line(aes(x = Week, y = model_mean), linetype = 2, color = "red") + 
  geom_line(aes(x = Week, y = median_age), linetype = 1, color = "black") + 
  geom_line(aes(x = Week, y = mean_age), linetype = 1, color = "red") +
  labs(title = "Hospitalizations", x = "Week", y = "Age") + 
  theme(plot.title = element_text(hjust = 0.5))

died_plot <- ggplot(data = died_join) +
  geom_line(aes(x = Week, y = model_median), linetype = 2, color = "black") + 
  geom_line(aes(x = Week, y = model_mean), linetype = 2, color = "red") + 
  geom_line(aes(x = Week, y = median_age), linetype = 1, color = "black") + 
  geom_line(aes(x = Week, y = mean_age), linetype = 1, color = "red") +
  labs(title = "Deaths", x = "Week", y = "Age") + 
  theme(plot.title = element_text(hjust = 0.5))

rchd_w_model <- plot_grid(case_plot, hosp_plot, died_plot, ncol = 1)
ggsave2("ts/alex/rchd-florida_with_model.png", plot = rchd_w_model, width = 7.57, height = 11, units = "in", dpi = 200)

case_plot2 <- ggplot(data = case_join) +
  geom_line(aes(x = Week, y = model_median, color = "Model Median Age", linetype = "Model Median Age")) + 
  geom_line(aes(x = Week, y = model_mean, color = "Model Mean Age", linetype = "Model Mean Age")) + 
  geom_line(aes(x = Week, y = median_age, color = "Linelist Median Age", linetype = "Linelist Median Age")) + 
  geom_line(aes(x = Week, y = mean_age, color = "Linelist Mean Age", linetype = "Linelist Mean Age")) +
  labs(title = "Cases", x = "Month", y = "Age") + 
  scale_color_manual(NULL, breaks = c("Model Median Age", "Model Mean Age", "Linelist Median Age", "Linelist Mean Age"), 
                     values = c("black", "red", "black", "red")) +
  scale_linetype_manual(NULL, breaks = c("Model Median Age", "Model Mean Age", "Linelist Median Age", "Linelist Mean Age"), 
                        values = c(2, 2, 1, 1)) +
  scale_x_continuous(breaks = wk_lab, labels = mth_lab) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.9, 0.8))

hosp_plot2 <- ggplot(data = hosp_join) +
  geom_line(aes(x = Week, y = model_median), linetype = 2, color = "black") + 
  geom_line(aes(x = Week, y = model_mean), linetype = 2, color = "red") + 
  geom_line(aes(x = Week, y = median_age), linetype = 1, color = "black") + 
  geom_line(aes(x = Week, y = mean_age), linetype = 1, color = "red") +
  labs(title = "Hospitalizations", x = "Month", y = "Age") + 
  scale_x_continuous(breaks = wk_lab, labels = mth_lab) +
  theme(plot.title = element_text(hjust = 0.5))

died_plot2 <- ggplot(data = died_join) +
  geom_line(aes(x = Week, y = model_median), linetype = 2, color = "black") + 
  geom_line(aes(x = Week, y = model_mean), linetype = 2, color = "red") + 
  geom_line(aes(x = Week, y = median_age), linetype = 1, color = "black") + 
  geom_line(aes(x = Week, y = mean_age), linetype = 1, color = "red") +
  labs(title = "Deaths", x = "Month", y = "Age") + 
  scale_x_continuous(breaks = wk_lab, labels = mth_lab) +
  theme(plot.title = element_text(hjust = 0.5))

rchd_w_model2 <- plot_grid(case_plot2, hosp_plot2, died_plot2, ncol = 1)
ggsave2("ts/alex/rchd-florida_with_model2.png", plot = rchd_w_model2, width = 7.57, height = 11, units = "in", dpi = 200)

fwrite(ll_case, "ts/age_rcase-florida.csv")
fwrite(ll_hosp, "ts/age_rhosp-florida.csv")
fwrite(ll_died, "ts/age_rdeath-florida.csv")
