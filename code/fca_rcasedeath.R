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

fwrite(df, outfile)
