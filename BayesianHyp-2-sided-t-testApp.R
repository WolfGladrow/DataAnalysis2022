print('file: BayesianHyp-2-side-t-testApp.R')
# 2-sample 2-sided t-test: artificial data: H0 is true
print(' ---------------------------------------------------')
print(' 0. Generate artificial data from normal PDF')
set.seed(1953)
mu = 1.8; sigma = 1.2 # true mean & standard deviation
n1 = 30; n2 = 32      # sample sizes
x1 = rnorm(n1,mu,sigma); x2 = rnorm(n2,mu,sigma); print('H0 is true')
print(' ---------------------------------------------------')
print(' 1. Apply conventional t-test (assume equal variances)')
pt = t.test(x1,x2,var.equal = TRUE)$p.value; print(c(round(pt,4),'pt'))
print(' ---------------------------------------------------')
print(' 2. Apply Welch t-test (variances may be unequal)')
ptWelch = t.test(x1,x2)$p.value; print(c(round(ptWelch,4),'ptWelch'))
print(' ---------------------------------------------------')
print(' 3. Apply Bayesian t-test: routine ttestBF, package BayesFactor')
# install.packages('BayesFactor')
library(BayesFactor)
outBF=ttestBF(x1,x2,rscale=1) # rscale=1 yields standard Cauchy prior
B10 = extractBF(outBF,onlybf = TRUE); print(c(round(B10,4),'B10'))
print(c(round(1/B10,4),'B01'))
print(' ---------------------------------------------------')
print(' 4. Bayesian t-test: Rouder et al. (2009)')
n = n1*n2/(n1+n2) # effective sample size (Rouder09 p.234)
nu = n1+n2-2      # degrees of freedom (Rouder09 p.234)
tval = t.test(x1,x2,var.equal = TRUE)$statistic # t-value
Rouder09 = function(g) {
  # Rouder et al. (2009, Eq.1)
  # input: 
  #   n = effective sample size
  #   nu = degrees of freedom
  #   tval = t-value of the conventional (frequentistic) t-test
  # output: integrand in denominator of Rouder09 Eq.1
  return((1+n*g)^(-1/2)*(1+tval^2/((1+n*g)*nu))^(-(nu+1)/2)*(2*pi)^(-1/2)*
           g^(-3/2)*exp(-1/(2*g)))
}
q = integrate(Rouder09,lower=0,upper=Inf)$value
B10 = q/(1+tval^2/nu)^(-(nu+1)/2); print(c(round(B10,4),'B10'))
B01 = 1/B10; print(c(round(B01,4),'B01'))
print(' ---------------------------------------------------')
print(' 5. Apply Bayesian t-test: routine meta.ttestBF, package BayesFactor')
metaBF = meta.ttestBF(tval,n1,n2,rscale=1)
B10 = extractBF(metaBF,onlybf = TRUE); print(c(round(B10,4),'B10'))
print(c(round(1/B10,4),'B01'))
## Using rscale=1 and one-sided test to be
## consistent with Rouder & Morey (2011)
