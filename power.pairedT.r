power.pairedT <- function(npair, rho, effectSize, sd, alpha, nsims=1000) {
  out <- replicate(nsims, {
    Sigma <- matrix(c(1, rho, rho, 1), 2, 2)
    Y <- matrix(rnorm(npair*2), npair, 2)
    Y <- Y %*% chol(Sigma) * sd
    Y[, 2] <- Y[, 2] + effectSize
    t.test(x=Y[, 1], y=Y[, 2], paired=TRUE)$p.value
  })
  mean(out < alpha)
}
