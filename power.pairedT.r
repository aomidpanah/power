power.pairedT <- function(npair, rho, effectSize, sd, alpha, nsims=1000) {
  Sigma <- matrix(c(1, rho, rho, 1), 2, 2)
  cSig <- chol(Sigma) * sd
  out <- replicate(nsims, {
    Y <- matrix(rnorm(npair*2), npair, 2)
    Y <- Y %*% cSig
    Y[, 2] <- Y[, 2] + effectSize
    t.test(x=Y[, 1], y=Y[, 2], paired=TRUE)$p.value
  })
  mean(out < alpha)
}
