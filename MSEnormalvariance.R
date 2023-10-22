print('MSEnormalvariance.R')
# Dieter.Wolf-Gladrow@awi.de 10/2023
# compare Monte Carlo simulation with analytical results
M = 1e4; print(c(M,'M')) # number of Monte Carlo runs
mu = 0.3; sigma = 1.1; sigmasq = sigma^2 # true mean, standard deviation, variance
n = 5 # sample size (small on purpose)
MSE1 = 0; MSE2 = 0
set.seed(1953) # set seed for random number generators
for(j in 1:M) {
  x = rnorm(n,mu,sigma) # random sample from normal PDF
  varx = var(x) # sample variance (with factor 1/(n-1))
  MSE1 = MSE1 + (varx-sigmasq)^2
  MSE2 = MSE2 + (varx*(n-1)/n-sigmasq)^2
}
MSE1 = MSE1/M; MSE2 = MSE2/M
MSE1a = 2*sigma^4/(n-1)      # analytical value (Casella & Berger, 2002, p.331)
MSE2a = sigma^4*(2*n-1)/n^2  # analytical value (Casella & Berger, 2002, p.331)
print(c(round(MSE1a,4),'MSE1a'))
print(c(round(MSE1,4),'MSE1'))
print(c(round(MSE2a,4),'MSE2a'))
print(c(round(MSE2,4),'MSE2'))
print(c(round(MSE1/MSE1a*100,2),'MSE1/MSE1a*100')) # numeric vs. analytical in %
print(c(round(MSE2/MSE2a*100,2),'MSE2/MSE2a*100'))