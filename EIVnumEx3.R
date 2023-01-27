print('file: EIVnumEx3.R')
print(date())
# Errors in variables (EIV): slope estimate (lambda)
# ----------------------------------------------------------------
# (1) Model parameters and generation of artificial data:
# ----------------------------------------------------------------
set.seed(1953)
SlopeTrue = 15 # true slope (N:P in mol/mol)
IcTrue = -2.5    # true intercept not equal 0 (negative -> at N=0 P is > 0)
n = 1e3    # sample size
sigxError = 0.5
sigyError = 14*sigxError # sigyError/sigxError close, however, not equal to SlopeTrue
# (1a) true P values from a normal distribution:
xiMean = 1.9    # mean xi concentration
sigxi = 0.7     # standard deviation
xiTrue = rnorm(n,xiMean,sigxi)
# (1b) remove negative values
tiny =  0.001; q = (xiTrue < tiny); xiTrue[q] = tiny
# (1c) true N values
yTrue = SlopeTrue*xiTrue+IcTrue
# (1d) add normal noise (independent from each other) with mean 0:
x = xiTrue+rnorm(n,0,sigxError)   # Pobs
y = yTrue+rnorm(n,0,sigyError)   # Nobs
# (1e) remove negative x, y values:
qx = (x < tiny); x[qx] = tiny
qy = (y < tiny); y[qy] = tiny
# ----------------------------------------------------------------
# (2) Estimate slope:
# ----------------------------------------------------------------
lambdaTrue = (sigxError/sigyError)^2 # variance ratio
lambda = lambdaTrue
data = data.frame(x,y)
CM = cov(data) # covariance matrix
Sxx = CM[1,1]; Syy = CM[2,2]; Sxy = CM[1,2]
lambdaArr = seq(lambdaTrue/5,lambdaTrue*5,lambdaTrue/10); L = length(lambdaArr)
SlopeMLEArr = numeric(L)
for(j in 1:L) {
  lambda = lambdaArr[j]
  SlopeMLEArr[j] = (-(Sxx-lambda*Syy)+sqrt((Sxx-lambda*Syy)^2+4*lambda*Sxy^2))/(2*lambda*Sxy)
  # IcMLE = mean(y)-SlopeMLE*mean(x)
}
# ----------------------------------------------------------------
# (3) Plot:
# ----------------------------------------------------------------
library(latex2exp)
sflag = 1
if (sflag == 1) { # beta(lambda)
  # png('EIVnutrientMC230126lambda.png',width=16,height=16,units='cm',res=300)
  plot(lambdaArr,SlopeMLEArr,type='l',lwd=3,col='blue',xlab=TeX('$\\lambda$'),
       ylab='ML estimate of slope',las=1,cex=0.6,cex.lab=1.5)
  abline(h=SlopeTrue,col='black')
  abline(v=lambdaTrue,col='black')
  # dev.off()
}