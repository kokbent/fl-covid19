#### Generate MD5 log for tgzs
tgzs <- list.files(path = ".", pattern = "*.tgz")

if (!file.exists("md5log")) {
  writeLines("time\tname\tmd5", "md5log")
}

md5log <- read.table("md5log", header = T, sep = "\t", stringsAsFactors = F)

for (f in tgzs) {
  md5 <- tools::md5sum(f)
  cond <- md5log$name == f & md5log$md5 == md5
  
  if (sum(cond) == 0) {
    ctime <- file.info(f)$ctime
    df <- data.frame(time = as.character(ctime), name = f, md5 = md5)
    md5log <- rbind(md5log, df)
  }
}

write.table(md5log, "md5log", sep = "\t", quote = F, row.names = F)