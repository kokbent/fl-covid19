# Instructions to update ts
* Download data table (.csv) from [CDC covid data tracker](https://covid.cdc.gov/covid-data-tracker/#trends_dailydeaths)
* At the webpage, change state to *Florida*, and View(left axis) to *Daily Cases* (this is the default). Scroll down to the *Data Table for Daily Death Trends - Florida* section, expand it, and click *Download Data* and download the csv file to data subfolder.
* Change View(left axis) to *Daily deaths*, do the same thing to download another csv file.
* **Update** the second date on line 64 of `code/fca_rcasedeath.R` to a date closer to current date. Generally I would choose the Saturday that is about 2 months from current date (This is for right truncation).
* Run the R script `code/fca_rcasedeath.R`
