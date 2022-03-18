print('file: SkedasticityTest.R')
# Test for homo- versus heteroskedasticity:
# (0) Generate artificial data:
beta0 = 8; beta = -1.2 # true intercept & slope
sigma = 1;              # true standard deviation of normal noise
set.seed(1953) # set seed for random number generators
n = 30   # sample size
dx = 4/(n-1); x = seq(1,5,dx); noise = sigma*rnorm(n); y = beta0 + beta*x + noise
# (1) linear regression & performing Non-Constant Error Variance Test ncvTest()
# install.packages('car')
library(car)
pskedasticity = ncvTest(lm(y ~ x))$p; print(c(round(pskedasticity,4),'p(skedasticity)'))
print('Null hypothesis H0: residuals are homoskedastic')
if (pskedasticity >= 0.05) print('Do not reject H0 on significance level alpha=0.05')
if (pskedasticity < 0.05)  print('Reject H0 on significance level alpha=0.05')