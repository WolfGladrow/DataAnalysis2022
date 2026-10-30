print(' ---------------------------------------------------')
print('file: Bayesian2ttest.R')
print('purpose: Bayesian 2-sample 2-sided t-test')
print('         based on Rouder et al. (2009)')
print(date())
print(R.Version()$version.string)
print(' ---------------------------------------------------')
print('Data/samples:')
print(' ---------------------------------------------------')
qflag = 3
print(c(qflag,'qflag'))
if (qflag == 4) {
print('Put (read in) your data here and set qflag to 3')
# n1 = length(x1); n2 = length(x2)
}
if (qflag == 3) {
  set.seed(1953)
  print('For qflag == 3 data are random numbers from')
  print('two normal distributions with (mu1=0,sigma=1)')
  print('and (mu2=1.25,sigma=2), respectively (borderline case).')
  mu1 = 0; sigma1 = 1 # true mean & standard deviation
  mu2 = 1.25; sigma2 = sigma1 # true mean & standard deviation
  n1 = 10; n2 = 9      # sample sizes
  # x1 = round(rnorm(n1,mu1,sigma1),4)
  # x2 = round(rnorm(n2,mu2,sigma2),4)
  print('borderline case')
  x1 = c(0.0219,-0.9043,0.4132,0.1866,0.2308,
         0.2357,1.4837,1.0995,-0.0047,-1.4212)
  x2 = c(1.1174,1.4148,1.1745,-1.0807,0.7268,
         -0.2678,2.0418,1.8202,3.7550)
  delta = abs(mu1-mu2)/max(sigma1,sigma2)
  deltaEstHedges = abs(mean(x1)-mean(x2))/
    sqrt(((n1-1)*var(x1)+(n2-1)*var(x2))/(n1+n2-2))
  # Hedges LV, Olkin I. Statistical methods for meta-analysis. 
  # San Diego, CA: Academic Press; 1985.
  print(c(round(delta,4),'effect size'))
  print(c(round(deltaEstHedges,4),'deltaEstHedges'))
}
if (qflag == 2) {
  print('For qflag == 2 data are random numbers from')
  print('two normal distributions with (mu1=0,sigma=1)')
  print('and (mu2=1.5,sigma=2), respectively.')
  mu1 = 0; sigma1 = 1 # true mean & standard deviation
  mu2 = 1.5; sigma2 = sigma1 # true mean & standard deviation
  n1 = 10; n2 = 9      # sample sizes
  # x1 = round(rnorm(n1,mu1,sigma1),4)
  # x2 = round(rnorm(n2,mu2,sigma2),4)
  x1 = c(0.26,0.2016,0.4736,0.4941,-0.3688,
         0.2365,0.6201,0.2098,-1.4904,-1.6032)     
  x2 = c(2.1542,1.3909,0.8515,2.2964,1.8177,
         1.7305,-0.0229,1.3014,-0.567)
  delta = abs(mu1-mu2)/max(sigma1,sigma2)
  deltaEstHedges = abs(mean(x1)-mean(x2))/
    sqrt(((n1-1)*var(x1)+(n2-1)*var(x2))/(n1+n2-2))
  print(c(round(delta,4),'effect size'))
  print(c(round(deltaEstHedges,4),'deltaEstHedges'))
}
if (qflag == 1) {
  print('For qflag == 1 all data are random numbers from')
  print('the normal distribution with mu=0 and sigma = 1')
  mu1 = 0; sigma1 = 1 # true mean & standard deviation
  mu2 = mu1; sigma2 = sigma1 # true mean & standard deviation
  n1 = 10; n2 = 9      # sample sizes
  # x1 = round(rnorm(n1,mu1,sigma1),4)
  # x2 = round(rnorm(n2,mu2,sigma2),4)
  x1 = c(0.0219,-0.9043,0.4132,0.1866,0.2308,
         0.2357,1.4837,1.0995,-0.0047,-1.4212)  
  x2 = c(-0.1326,0.1648,-0.0755,-2.3307,-0.5232,
         -1.5178,0.7918,0.5702,2.505)
  delta = abs(mu1-mu2)/max(sigma1,sigma2)
  deltaEstHedges = abs(mean(x1)-mean(x2))/
    sqrt(((n1-1)*var(x1)+(n2-1)*var(x2))/(n1+n2-2))
  print(c(round(delta,4),'effect size'))
  print(c(round(deltaEstHedges,4),'deltaEstHedges'))
}
n1 = length(x1); n2 = length(x2)
print(' ---------------------------------------------------')
print(' 1. Apply (conventional) Null Hypothesis Significance Test')
print(' (NHST) 2-sided t-test (assuming equal variances of populations).')
print('Null hypothesis H0: mean population values are equal (mu1=mu2).')
pt = t.test(x1,x2,var.equal=TRUE)$p.value 
print(c(round(pt,4),'pt'))
print(' Set (choose!) level of significance alpha:')
alpha = 0.05 
if(pt < alpha) print('Reject H0')
if(pt >= alpha) print('Do not reject H0')
print(' ---------------------------------------------------')
print(' 2. Apply Welch t-test (variances may be unequal)')
ptWelch = t.test(x1,x2)$p.value
print(c(round(ptWelch,4),'ptWelch'))
if(ptWelch < alpha) print('Reject H0')
if(ptWelch >= alpha) print('Do not reject H0')
print(' ---------------------------------------------------')
print(' 3. Apply Bayesian t-test: routine ttestBF, package BayesFactor')
# install.packages('BayesFactor')
library(BayesFactor)
outBF=ttestBF(x1,x2,rscale=1) # rscale=1 yields standard Cauchy prior
B10 = extractBF(outBF,onlybf = TRUE); print(c(round(B10,4),'B10'))
print(c(round(1/B10,4),'B01'))
if(B10 > 10)                     print('strong evidence against H0/strong evidence for H1')
if((B10 >= 3.16) && (B10 <= 10)) print('substantial evidence against H0/substantial evidence for H1')
if((B10 > 1) && (B10 < 3.16))    print('slight evidence against H0/slight evidence for H1')
if((B10 >= 0.316) && (B10 <= 1)) print('slight evidence against H1/slight evidence for H0')
if((B10 > 0.1) && (B10 < 0.316)) print('substantial evidence against H1/substantial evidence for H0')
if(B10 <= 0.1)                   print('strong evidence against H1/strong evidence for H0')
print(' ---------------------------------------------------')
print(' 4. Bayesian t-test: Rouder et al. (2009)')
n = n1*n2/(n1+n2) # effective sample size (Rouder09 p.234)
nu = n1+n2-2      # degrees of freedom (Rouder09 p.234)
tval = t.test(x1,x2,var.equal=TRUE)$statistic # t-value
print(c(n,'n effective sample size'))
print(c(nu,'nu degrees of freedom'))
print(c(tval,'tval'))
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
print(c(round(q,4),'q')) 
B10 = q/(1+tval^2/nu)^(-(nu+1)/2)
print(c(round(B10,4),'B10')) # should be identical to output
                             # from ttestBF(x1,x2,rscale=1)             
B01 = 1/B10; print(c(round(B01,4),'B01'))
print(' ---------------------------------------------------')
print(' 5. Apply Bayesian t-test: routine meta.ttestBF, package BayesFactor')
metaBF = meta.ttestBF(tval,n1,n2,rscale=1)
B10 = extractBF(metaBF,onlybf = TRUE)
print(c(round(B10,4),'B10'))
print(c(round(1/B10,4),'B01')) # should be identical to output
                               # from ttestBF(x1,x2,rscale=1) 
# --------------------------------------------------------------
# Remarks:
# (1) The R routine ttestBF() and my (pedestrian) integration
#     yield identical values for the Bayes factor B10
# (2) For qflag = 1 (samples from identical populations) the
#     conclusion 
#     'slight evidence against H1/slight evidence for H0'
#     sounds a bit weak. The reason is the small size of the samples.
# --------------------------------------------------------------
# 
# [1] " ---------------------------------------------------"
# [1] "file: Bayesian2ttest.R"
# [1] "purpose: Bayesian 2-sample 2-sided t-test"
# [1] "         based on Rouder et al. (2009)"
# [1] "Fri Oct 30 08:30:26 2026"
# [1] "R version 4.4.2 (2024-10-31)"
# [1] " ---------------------------------------------------"
# [1] "Data/samples:"
# [1] " ---------------------------------------------------"
# [1] "1"     "qflag"
# [1] "For qflag == 1 all data are random numbers from"
# [1] "the normal distribution with mu=0 and sigma = 1"
# [1] "0"           "effect size"
# [1] "0.1729"         "deltaEstHedges"
# [1] " ---------------------------------------------------"
# [1] " 1. Apply (conventional) Null Hypothesis Significance Test"
# [1] " (NHST) 2-sided t-test (assuming equal variances of populations)."
# [1] "Null hypothesis H0: mean population values are equal (mu1=mu2)."
# [1] "0.7114" "pt"    
# [1] " Set (choose!) level of significance alpha:"
# [1] "Do not reject H0"
# [1] " ---------------------------------------------------"
# [1] " 2. Apply Welch t-test (variances may be unequal)"
# [1] "0.7197"  "ptWelch"
# [1] "Do not reject H0"
# [1] " ---------------------------------------------------"
# [1] " 3. Apply Bayesian t-test: routine ttestBF, package BayesFactor"
# [1] "0.334" "B10"  
# [1] "2.9941" "B01"   
# [1] "slight evidence against H1/slight evidence for H0"
# [1] " ---------------------------------------------------"
# [1] " 4. Bayesian t-test: Rouder et al. (2009)"
# [1] "4.73684210526316"        "n effective sample size"
# [1] "17"                    "nu degrees of freedom"
# t                     
# "0.376208477997117"              "tval" 
# [1] "0.31" "q"   
# t         
# "0.334"   "B10" 
# t          
# "2.9941"    "B01" 
# [1] " ---------------------------------------------------"
# [1] " 5. Apply Bayesian t-test: routine meta.ttestBF, package BayesFactor"
# [1] "0.334" "B10"  
# [1] "2.9941" "B01"
# --------------------------------------------------------
# --------------------------------------------------------
# --------------------------------------------------------

# [1] " ---------------------------------------------------"
# [1] "file: Bayesian2ttest.R"
# [1] "purpose: Bayesian 2-sample 2-sided t-test"
# [1] "         based on Rouder et al. (2009)"
# [1] "Fri Oct 30 08:32:10 2026"
# [1] "R version 4.4.2 (2024-10-31)"
# [1] " ---------------------------------------------------"
# [1] "Data/samples:"
# [1] " ---------------------------------------------------"
# [1] "2"     "qflag"
# [1] "For qflag == 2 data are random numbers from"
# [1] "two normal distributions with (mu1=0,sigma=1)"
# [1] "and (mu2=1.5,sigma=2), respectively."
# [1] "1.5"         "effect size"
# [1] "1.4766"         "deltaEstHedges"
# [1] " ---------------------------------------------------"
# [1] " 1. Apply (conventional) Null Hypothesis Significance Test"
# [1] " (NHST) 2-sided t-test (assuming equal variances of populations)."
# [1] "Null hypothesis H0: mean population values are equal (mu1=mu2)."
# [1] "0.0051" "pt"    
# [1] " Set (choose!) level of significance alpha:"
# [1] "Reject H0"
# [1] " ---------------------------------------------------"
# [1] " 2. Apply Welch t-test (variances may be unequal)"
# [1] "0.0059"  "ptWelch"
# [1] "Reject H0"
# [1] " ---------------------------------------------------"
# [1] " 3. Apply Bayesian t-test: routine ttestBF, package BayesFactor"
# [1] "9.2404" "B10"   
# [1] "0.1082" "B01"   
# [1] "substantial evidence against H0/substantial evidence for H1"
# [1] " ---------------------------------------------------"
# [1] " 4. Bayesian t-test: Rouder et al. (2009)"
# [1] "4.73684210526316"        "n effective sample size"
# [1] "17"                    "nu degrees of freedom"
# t                     
# "-3.21377504403763"              "tval" 
# [1] "0.1289" "q"     
# t          
# "9.2404"    "B10" 
# t          
# "0.1082"    "B01" 
# [1] " ---------------------------------------------------"
# [1] " 5. Apply Bayesian t-test: routine meta.ttestBF, package BayesFactor"
# [1] "9.2404" "B10"   
# [1] "0.1082" "B01"   
# --------------------------------------------------------
# --------------------------------------------------------
# --------------------------------------------------------

# [1] " ---------------------------------------------------"
# [1] "file: Bayesian2ttest.R"
# [1] "purpose: Bayesian 2-sample 2-sided t-test"
# [1] "         based on Rouder et al. (2009)"
# [1] "Fri Oct 30 08:35:09 2026"
# [1] "R version 4.4.2 (2024-10-31)"
# [1] " ---------------------------------------------------"
# [1] "Data/samples:"
# [1] " ---------------------------------------------------"
# [1] "3"     "qflag"
# [1] "For qflag == 3 data are random numbers from"
# [1] "two normal distributions with (mu1=0,sigma=1)"
# [1] "and (mu2=1.25,sigma=2), respectively (borderline case)."
# [1] "borderline case"
# [1] "1.25"        "effect size"
# [1] "0.9351"         "deltaEstHedges"
# [1] " ---------------------------------------------------"
# [1] " 1. Apply (conventional) Null Hypothesis Significance Test"
# [1] " (NHST) 2-sided t-test (assuming equal variances of populations)."
# [1] "Null hypothesis H0: mean population values are equal (mu1=mu2)."
# [1] "0.0577" "pt"    
# [1] " Set (choose!) level of significance alpha:"
# [1] "Do not reject H0"
# [1] " ---------------------------------------------------"
# [1] " 2. Apply Welch t-test (variances may be unequal)"
# [1] "0.0689"  "ptWelch"
# [1] "Do not reject H0"
# [1] " ---------------------------------------------------"
# [1] " 3. Apply Bayesian t-test: routine ttestBF, package BayesFactor"
# [1] "1.4563" "B10"   
# [1] "0.6867" "B01"   
# [1] "slight evidence against H0/slight evidence for H1"
# [1] " ---------------------------------------------------"
# [1] " 4. Bayesian t-test: Rouder et al. (2009)"
# [1] "4.73684210526316"        "n effective sample size"
# [1] "17"                    "nu degrees of freedom"
# t                     
# "-2.03527440453107"              "tval" 
# [1] "0.2046" "q"     
# t          
# "1.4563"    "B10" 
# t          
# "0.6867"    "B01" 
# [1] " ---------------------------------------------------"
# [1] " 5. Apply Bayesian t-test: routine meta.ttestBF, package BayesFactor"
# [1] "1.4563" "B10"   
# [1] "0.6867" "B01"   
# --------------------------------------------------------
# --------------------------------------------------------
# --------------------------------------------------------
  