#### Death by report day
library(dplyr)
library(data.table)
library(lubridate)

daily <- fread("data/FCA_daily.csv")
cnt <- fread("data/FCA_county.csv")

daily_w_cnt <- cnt %>%
  select(DEPCODE, County = County_1) %>%
  right_join(daily)
daily_w_cnt$Date <- ymd_hms(daily_w_cnt$Date) %>% as.Date()

#### Choice of counties
df <- daily_w_cnt %>%
  filter(County != "Unknown") %>%
  group_by(Date) %>%
  summarise(rcase = sum(cases), rdeath = sum(deaths_rep)) %>%
  arrange(Date)

df <- df %>%
  filter(Date <= ymd("2021-05-26"))
outfile <- "ts/rcasedeath-florida.csv"
fwrite(df, outfile)

#### Merging with CDC data
cdc_data <- fread("data/data_table_for_daily_case_trends__florida.csv")
cdc_data$Date <- mdy(cdc_data$Date)
cdc_data <- cdc_data %>%
  filter(Date > ymd("2021-05-26")) %>%
  select(Date, rcase = `New Cases`)
df <- bind_rows(df, cdc_data)
df <- df %>%
  arrange(Date)
plot(df$Date, df$rcase, type="l")

cdc_death <- fread("data/data_table_for_daily_death_trends__florida.csv")
cdc_death$Date <- mdy(cdc_death$Date)
cdc_death <- cdc_death %>%
  select(Date, death_incd = `New Deaths`)

df <- df %>%
  left_join(cdc_death)

#### Excess death
excess <- fread("https://data.cdc.gov/api/views/xkkf-xrst/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=")
excess1 <- excess %>%
  filter(State == "Florida", Type == "Predicted (weighted)", Outcome == "All causes") %>%
  mutate(week_end_date = ymd(`Week Ending Date`),
         Week = epiweek(week_end_date),
         Year = ifelse(Week %in% 52:53 & month(week_end_date) == 1,
                       year(week_end_date) - 1,
                       year(week_end_date)))

excess1 <- excess1 %>%
  group_by(week_end_date, Week, Year) %>%
  summarise(n = sum(`Observed Number`))

excess2 <- excess1 %>%
  group_by(Week) %>%
  mutate(exp_n = mean(n[Year < 2020])) %>%
  ungroup() %>%
  mutate(exp_n = ifelse(!is.na(exp_n), exp_n, exp_n[Week == 52][1])) %>%
  filter(week_end_date >= ymd("2020-01-11"), week_end_date <= ymd("2022-03-19")) %>%
  mutate(diff = n - exp_n,
         base_diff = mean(diff[week_end_date <= ymd("2020-03-28")]),
         adj_diff = diff - base_diff)

df1 <- df %>%
  mutate(Week = epiweek(Date),
         Year = ifelse(Week %in% 52:53 & month(Date) == 1,
                       year(Date) - 1,
                       year(Date)))

df1 <- df1 %>%
  group_by(Year, Week) %>%
  mutate(week_end_date = max(Date))
df2 <- left_join(df1, 
                 excess2 %>% select(week_end_date, excess=adj_diff)) %>%
  mutate(excess = excess / 7)

plot(df2$Date, df2$excess, type = "l")

fwrite(df2 %>% ungroup %>%
         select(-Week, -Year, -week_end_date), outfile)
