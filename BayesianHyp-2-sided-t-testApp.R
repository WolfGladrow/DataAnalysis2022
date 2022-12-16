print('file: BayesianHyp-2-side-t-testApp.R')
print(date())
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
# Jeffreys' scales of evidence (slightly modified):
BF = B10
if(BF > 10)                    print('strong evidence against H0/strong evidence for H1')
if((BF >= 3.16) && (BF <= 10)) print('substantial evidence against H0/substantial evidence for H1')
if((BF > 1) && (BF < 3.16))    print('slight evidence against H0/slight evidence for H1')
if((BF >= 0.316) && (BF <= 1)) print('slight evidence against H1/slight evidence for H0')
if((BF > 0.1) && (BF < 0.316)) print('substantial evidence against H1/substantial evidence for H0')
if(BF <= 0.1)                  print('strong evidence against H1/strong evidence for H0')
# -----------------------------------------------------------------------------
# Results:
# "file: BayesianHyp-2-side-t-testApp.R"
# "Fri Dec 16 08:51:03 2022"
# " ---------------------------------------------------"
# " 0. Generate artificial data from normal PDF"
# "H0 is true"
# " ---------------------------------------------------"
# " 1. Apply conventional t-test (assume equal variances)"
# "0.7788" "pt"    
# " ---------------------------------------------------"
# " 2. Apply Welch t-test (variances may be unequal)"
# "0.7787"  "ptWelch"
# " ---------------------------------------------------"
# " 3. Apply Bayesian t-test: routine ttestBF, package BayesFactor"
# 
# ************
#   Welcome to BayesFactor 0.9.12-4.2. If you have questions, please contact Richard Morey (richarddmorey@gmail.com).
# 
# Type BFManual() to open the manual.
# ************
#   [1] "0.1987" "B10"   
# "5.0319" "B01"   
# " ---------------------------------------------------"
# " 4. Bayesian t-test: Rouder et al. (2009)"
# t          
# "0.1987"    "B10" 
# t          
# "5.0319"    "B01" 
# " ---------------------------------------------------"
# " 5. Apply Bayesian t-test: routine meta.ttestBF, package BayesFactor"
# "0.1987" "B10"   
# "5.0319" "B01"
# "substantial evidence against H1/substantial evidence for H0"
# -----------------------------------------------------------------------------
# Remarks:
# The (frequentistic) t-test yields a p-value of 0.7788 and thus
#   H0 is not rejected on the level of significance alpha = 0.05.
# The (frequentistic) Welch t-test yields (not assuming equal
#   variances) yields a only slightly different p-value (0.7787).
#   Again: H0 is not rejected on the level of significance alpha = 0.05.
# The Bayesian-t-test yields a Bayes factor B01 of 5.0319 which
#   (applying a slightly modified form of Jeffreys' scales of evidence)
#   provides substantial evidence against H1 and substantial 
#   evidence for H0.
# Note that H1 is not tested in the frequentistic approach.
# -----------------------------------------------------------------------------