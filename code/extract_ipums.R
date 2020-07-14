# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(readr)

ddi <- read_ipums_ddi("C:/Users/kokbent/Dropbox (UFL)/COVID-FL/population_density/usa_00002.xml")
data <- read_ipums_micro(ddi)

colnames(data)

data1 <- data[,c("YEAR", "SAMPLE", "SERIAL", "HHWT", "COUNTYFIP", "PUMA", "PERNUM", "PERWT", 
                 "SEX", "AGE", "SCHOOL", "EMPSTAT", "EMPSTATD", "PWSTATE2", "PWCOUNTY", 
                 "PWPUMA00", "TRANWORK", "TRANTIME", "GQ")]
data1$PUMA <- as.numeric(data1$PUMA)
head(data1)

tapply(data1$PERWT, data1$COUNTYFIP, sum)
length(tapply(data1$PERWT, data1$COUNTYFIP, sum))

tmp <- tapply(data1$PERWT, data1$PUMA, sum)
length(tmp)
sum(data1$PERWT)

write_rds(data1, "./tmp/IPUMS_ACR_2014-18.rds")
