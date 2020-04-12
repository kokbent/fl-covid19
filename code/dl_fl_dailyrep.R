curl::curl_download("https://floridadisaster.org/globalassets/covid19/dailies/covid-19-data---daily-report-2020-04-10-1712.pdf",
                          destfile = "etc/2020-04-10.pdf")

dl <- curl::curl_download("https://floridadisaster.org/globalassets/covid-19-data---daily-report-2020-03-24-1657.pdf",
                          destfile = "etc/2020-03-24.pdf")
tme <- c(as.character(1830:1800), as.character(1759:1700), as.character(1659:1600))
tme <- rev(tme)
tme <- c(paste0("0", as.character(959:930)), as.character(1059:1000))

d <- "2020-04-10"
url1 <- "https://floridadisaster.org/globalassets/covid19/dailies/covid-19-data---daily-report-"
url2 <- paste0(d, "-")

for (i in 1:length(tme)) {
  print(tme[i])
  url <- paste0(url1, url2, tme[i], ".pdf")
  dl <- curl::curl_download(url, destfile =paste0("etc/", d, ".pdf"))
  if (file.info(dl)["size"] < 100000) {
    print(file.info(dl)["size"])
    file.remove(dl)
  } else {
    break
  }
}
