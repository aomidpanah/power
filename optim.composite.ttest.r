optim.composite.ttest <- function(maxtest, maxn, crange=1:10, d, s, sig.level=0.05, tside=2, ...) {  
  optim.test <- function(c) {
    actualn <- min(ceiling(maxn / c), maxtest)*c
    power.t.test.composite(n1=floor(actualn/2), c1=c, d=d, s1=s, sig.level=sig.level, tside=tside, log=TRUE)
  }
  plot(exp(sapply(crange, optim.test)), xlab='Composite size', ylab='Power')
}
