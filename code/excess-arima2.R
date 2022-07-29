library(dplyr)
library(data.table)
library(lubridate)
library(forecast)
library(ggplot2)

#### INPUT ----
## get data from cdc
excess <- fread("https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=")

## calculate epiweek
excess1 <- excess %>%
  filter(Jurisdiction == "Florida", Type == "Predicted (weighted)") %>%
  mutate(week_end_date = mdy(`Week Ending Date`),
         Week = epiweek(week_end_date),
         Year = ifelse(Week %in% 52:53 & month(week_end_date) == 1,
                       year(week_end_date) - 1,
                       year(week_end_date)))

#### FUNCTIONS ----
get_excess <- function (df) {
  df1 <- df %>%
    filter(Week != 53)
  
  death_ts <- ts(df$n, frequency = 52, 
                 start = c(2015, 1), end = c(2022, 20))
  
  mod <- window(death_ts, start = c(2015, 1), end = c(2020, 10)) %>%
    auto.arima
  pred52 <- forecast(mod, 114)
  
  pred_df <- data.frame(
    year = floor(time(pred52$mean)),
    week = cycle(pred52$mean),
    expect = pred52$mean
  ) %>%
    mutate(year = ifelse(week == 1, year + 1, year)) 
  tmp <- pred_df %>% filter(year == 2020, week == 52)
  tmp$week <- 53
  pred_df <- pred_df %>%
    bind_rows(tmp) %>%
    arrange(year, week)
  
  df_fin <- df %>%
    left_join(pred_df, by = c("Week" = "week",
                             "Year" = "year")) %>%
    filter(!is.na(expect)) %>%
    mutate(excess = n - expect)
  
  return(df_fin)
}


#### ALL AGE GROUP ----
df_all <- excess1 %>%
  group_by(week_end_date, Week, Year) %>%
  summarise(n = sum(`Number of Deaths`))

excess_all <- get_excess(df_all)
excess_all$Age <- "All age"

#### Sub age group ----
agegs <- excess1$`Age Group` %>% unique
bigdf <- data.frame()
for (a in agegs) {
  df_a <- excess1 %>%
    filter(`Age Group` == a) %>%
    group_by(week_end_date, Week, Year) %>%
    summarise(n = sum(`Number of Deaths`))
  
  excess_a <- get_excess(df_a)
  excess_a$Age <- a
  bigdf <- bind_rows(bigdf, excess_a)
}

bigdf1 <- bind_rows(bigdf, excess_all)

ggplot(bigdf1) +
  geom_line(aes(x=week_end_date, y=excess, colour=Age))
