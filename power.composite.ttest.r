power.t.test.composite <- function(n1, n2=NULL, c1, c2=NULL, d, s1, s2=NULL, sig.level=0.05, tside=2, ...) {
  if (is.null(n2)) n2 <- n1
  if (is.null(c2)) c2 <- c1
  if (is.null(s2)) s2 <- s1
  
  ncp <- d / sqrt(s1^2/n1 + s2^2/n2)
  edf <- n1%/%c1 + n2%/%c2 - 2
  pt(qt(1-sig.level/tside, edf, lower = TRUE), df=edf, ncp=ncp, lower=FALSE, ...)
}
