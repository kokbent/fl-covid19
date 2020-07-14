ltarget_ <- function (y) function (alpha) {
  xm <- 1
  bp1 <- c(1, 5, 10, 20, 50, 100, 250, 500, 1000)
  bp2 <- c(bp1[-1] - 1, 50000)
  bin <- cbind(bp1, bp2)
  lpdf <- apply(bin, 1, function (x) matrixStats::logSumExp(log(alpha) + alpha * log(xm) - (alpha+1) * log(x[1]:x[2])))
  lprob <- lpdf - matrixStats::logSumExp(lpdf)
  
  tl <- sum(y * lprob)
  tl
}

ltarget2_ <- function (y) function (alpha) {
  xm <- 1
  bp1 <- c(1, 5, 10, 20, 50, 100, 250, 500, 1000)
  bp2 <- c(bp1[-1] - 1, 50000)
  bin <- cbind(bp1, bp2)
  lpdf <- apply(bin, 1, function (x) matrixStats::logSumExp(dexp(x[1]:x[2] - 1, alpha, log=T)))
  lprob <- lpdf - matrixStats::logSumExp(lpdf)
  
  tl <- sum(y * lprob)
  tl
}

ltarget3_ <- function (y) function (p) {
  xm <- 1
  bp1 <- c(1, 5, 10, 20, 50, 100, 250, 500, 1000)
  bp2 <- c(bp1[-1] - 1, 20000)
  bin <- cbind(bp1, bp2)
  lpdf <- apply(bin, 1, function (x) matrixStats::logSumExp(dnbinom(x[1]:x[2] - 1, p[1], p[2], log=T)))
  lprob <- lpdf - matrixStats::logSumExp(lpdf)
  
  tl <- sum(y * lprob)
  -tl
}

lgpd <- function (x, s, xi, mu = 0.5) {
  z <- (x - mu)/s
  ld <- -log(s) - (1/xi + 1) * log(1 + xi * z)
  ld <- ifelse(is.na(ld), -Inf, ld)
  return(ld)
}

lgpd_target_ <- function (y) function (p, mu = 0.5) {
  bp1 <- c(1, 5, 10, 20, 50, 100, 250, 500, 1000)
  bp2 <- c(bp1[-1] - 1, 20000)
  bin <- cbind(bp1, bp2)
  lpdf <- apply(bin, 1, function (x) matrixStats::logSumExp(lgpd(x[1]:x[2], p[1], p[2], mu)))
  lprob <- lpdf - matrixStats::logSumExp(lpdf)
  
  tl <- sum(y * lprob)
  -tl
}

rgpd <- function (n, s, xi, mu = 0.5) {
  return(mu + s * (runif(n)^-xi - 1) / xi)
}

two_stage_samp <- function (n, grp, s, xi) {
  bp1 <- c(1, 5, 10, 20, 50, 100, 250, 500, 1000)
  bp2 <- c(bp1[-1] - 1, 50000)
  bin <- cbind(bp1, bp2)
  
  g_samp <- rmultinom(100, n, grp)
  ind <- which(rowSums(g_samp) != 0)
  
  samp <- c()
  for (i in 1:length(ind)) {
    g <- ind[i]
    nsamp <- sum(g_samp[g,])
    choice <- bin[g,1]:bin[g,2]
    lpdf <- lgpd(choice, s, xi)
    if(any(is.na(lpdf))) print(paste0(g, nsamp, s, xi))
    lprob <- lpdf - matrixStats::logSumExp(lpdf)
    if(any(is.na(lprob))) print(paste0(g, nsamp, s, xi))
    r <- sample(choice, nsamp, replace = T, prob = exp(lprob))
    samp <- rbind(samp, cbind(r, rep(1:100, g_samp[g,])))
  }
  
  samp <- samp[order(samp[,2]),] %>%
    matrix(n, 100)
  if (n != 1) {
    samp <- apply(samp, 2, sample)
  }
  
  return(samp)
}

two_stage_sample <- function (grpmat, s, xi) {
  grp <- apply(grpmat, 1, 
               function (x) sample(1:ncol(grpmat), 100, replace = T, prob = x)) %>%
    t
  
  
  bp1 <- c(1, 5, 10, 20, 50, 100, 250, 500, 1000)
  bp2 <- c(bp1[-1] - 1, 50000)
  bin <- cbind(bp1, bp2)
}