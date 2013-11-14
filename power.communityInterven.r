## program:
## purpose: community intervention setting where intervention is rolled out 3 years after control
##          follow-up. There is unknown attrition and we are interested in estimating the effect
##          of intervention accounting for time dependent trends

nyear <- 4
year <- seq(0, nyear-1)
denom <- rep(300, nyear)
alpha.lintrend <- log(0.10)
beta.lintrend <- log(1.20)
beta.interven <- log(1.50)
alpha.atrition <- log(0.30)
beta.atrition <- log(1)

sim.dat <- function(nyear, year, denom, alpha.lintrend, beta.lintrend, beta.interven, alpha.atrition, beta.atrition) {
vaccrate <- exp(alpha.lintrend + year*beta.lintrend + beta.interven*(year=={nyear-1}))
attrrate <- exp(alpha.atrition + year*beta.atrition)

numer <- rbinom(nyear, denom, vaccrate)

denom.obs <- rbinom(nyear, denom, 1-attrrate)
numer.obs <- rbinom(nyear, denom.obs, numer/denom)

denom.cum <- cumsum(denom.obs)
numer.cum <- cumsum(numer.obs)
return(cbind('y'=numer.cum, 'n'=denom.cum, 'z'=denom.cum-numer.cum, 'p'=numer.cum/denom.cum))
}

OUT <- replicate(1000, sim.dat(nyear, year, denom, alpha.lintrend, beta.lintrend, beta.interven,
  alpha.atrition, beta.atrition))

x <- c(0, 0, 0, 1)

## simulated outcomes
plot.new()
plot.window(xlim=range(year), ylim=range(OUT[, 'p', ]))
apply(OUT[, 'p', ], 2, lines, x=year, col=rgb(0, 0, 0, .1))
axis(1, at=year, labels=year)
axis(2)

res <- apply(OUT, 3, function(out) {
  out <- as.data.frame(out)
  fit1 <- glm(cbind(y, z) ~ x, family=binomial(link=log), data=out)
  fit2 <- glm(cbind(y, z) ~ year + x, family=binomial(link=log), data=out)
  fit3 <- glm(cbind(y, z) ~ year + x, family=quasibinomial(link=log), data=out)
  ##fit4 <- glmer(cbind(y, z) ~ x + (1|year), family=binomial, data=out)
  fit4 <- glmer(cbind(y, z) ~ x + (1|year), family=binomial, data=out)


  cbind(
    'naive'=confint.default(fit1)['x', ],
    'temporal'=confint.default(fit2)['x', ],
    'quasibin'=confint.default(fit3)['x', ]
  )
})

cover.naive <- mean(res[1, ] < beta.interven & res[2, ] > beta.interven)
cover.satur <- mean(res[3, ] < beta.interven & res[4, ] > beta.interven)
cover.quasi <- mean(res[5, ] < beta.interven & res[6, ] > beta.interven)

c(cover.naive, cover.satur, cover.quasi)
