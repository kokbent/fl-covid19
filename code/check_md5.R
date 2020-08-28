#### Simple check MD5 utility
args <- commandArgs(trailingOnly = T)

## Check args
if(length(args) == 1) {
  infile <- args
} else {
  stop("Usage: Rscript check_md5.R [target file]")
}

## Check md5log
if (!file.exists("md5log")) {
  stop("md5log file not found in current directory.")
}

## Check input file
if (!file.exists(infile)) {
  stop("Input file not found.")
}

## Match MD5
md5 <- tools::md5sum(infile)
md5log <- read.table("md5log", header = T, sep = "\t", stringsAsFactors = F)

cond1 <- sapply(md5log$name, function (x) length(grep(x, infile)) > 0)
cond2 <- md5log$md5 == md5

if (sum(cond1 & cond2) == 1) {
  cat("Check sum PASS.\n")
  if(sum(cond1) > 1) {
    times <- as.POSIXct(md5log$time[cond1])
    if (max(times) == as.POSIXct(md5log$time[cond1 & cond2])) {
      cat("Multiple entries of same file name detected, but yours is the newest.\n")
    } else {
      warning("Multiple entries of same file name detected, yours is not the newest.\n")
    }
  }
  
  if(sum(cond2) > 1) {
    nama <- md5log$name[cond2]
    nama_collapse <- paste(nama, collapse = " ")
    warning(paste0("MD5 matches with more than one entry. Very concerning...\n", 
                   nama_collapse, "\n"))
  }
} else if (sum(cond1 & cond2) == 0) {
  cat("Check sum FAIL.\n")
  if(sum(cond1) > 0) warning("Filename exists in the log but MD5 not matched.")
  
  if(sum(cond2) > 0) warning("MD5 exists in the log but filename not matched.")
  
  if(sum(cond1) == 0 & sum(cond2) == 0) warning("Neither filename nor MD5 matched anything in the log.")
} else {
  cat("Check sum PASS.\n")
  warning("But more than one entry in the log has the same filename and MD5 combinations. Very concerning...")
}