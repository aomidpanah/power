## program: power.prop.test.unevenN
## purpose: calculate the power for a simple proportions test based on normal asymptotic distn of sample proportion
## author : adam omidpanah
## date   : 11-07-2013

## about:
##   the defacto in power analyses is to calculate even N. It's assumed that there's never any justification of ever
##   having or using uneven N. If you do obtain such uneven N, it's safe to say, "assume the smallest N is the effective
##   N in each group". That's not true, and that's a very stupid way to go about things. You can use a naive pooled 
##   variance and, in the case that n1, n2 both very small, and p1 very different from p2, you can get better powered 
##   tests by randomizing (or sampling) in an imbalance. A necessary assumption to make this work is that you can actually
##   calculate the variance under the alternative hypothesis. That means that this test is a version of the Wald test and
##   not the usual score test which has pooled variance in the denominator of the test statistic.


power.prop.test.unevenN <- function(n1, n2, p1, p2, alpha=0.05) {
  pdiff <- p1 - p2
  varh1 <- p1*(1-p1)/n1 + p2*(1-p2)/n2
  noncp <- pdiff^2/varh1
  critv <- qchisq(alpha, 1, 0, lower.tail=FALSE)
  pchisq(critv, 1, noncp, lower.tail=FALSE)
}
