print('file: EIVartificial1.R')
print(date())
# purpose: Errors in variables: use artificial data to investigate various methods
# created by: Dieter.Wolf-Gladrow@awi.de 
#    2/2023 version 1.0
# ----------------------------------------------------------------------------------
# -----------------------------------------------------------
# (1) Generate artificial data that look a bit similar to N:P data (Redfield)
# -----------------------------------------------------------
  set.seed(1953)
  SlopeTrue = 15 # true slope (N:P in mol/mol)
  IcTrue = -2.5    # true intercept not equal 0 (negative -> P is > 0 at N=0)
  n = 1e3    # sample size
  sigx = 0.5
  sigy = 5*sigx # sigy/sigx is not equal to 1 or equal to SlopeTrue
  lambdaTrue = (sigx/sigy)^2 # ratio of error variances (def. according to
                             # Casella & Berger, 2002)
  # (1a) true xi (<-> PO4) values from a normal distribution:
  ximean = 1.9    # mean x concentration
  xisigma = 0.7   # standard deviation
  xi = rnorm(n,ximean,xisigma)
  # (1b) true zeta (<-> NO3) values
  zeta = SlopeTrue*xi+IcTrue
  # (1c) add normal noise (independent from each other) with mean 0:
  errx = rnorm(n,0,sigx)
  erry = rnorm(n,0,sigy)
  x = xi+errx     # 'observed' values
  y = zeta+erry   # 'observed' values
  # -----------------------------------------------------------
  # (2) Simple linear regression (SLR): y on x
  # -----------------------------------------------------------
  SLRyonx = lm(y ~ x)
  SlopeYonX = as.numeric(SLRyonx$coefficients[2])
  IcYonX = as.numeric(SLRyonx$coefficients[1])
  # -----------------------------------------------------------
  # (3) Bayesian linear regression (BLR): y on x
  # -----------------------------------------------------------
  # MCMCregress() (MCMCpack)
  # BLR() (BLR)
  # install.packages('MCMCpack') # Markov Chain Monte Carlo (MCMC) Package
  library(MCMCpack)
  # install.packages('BLR') # Bayesian Linear Regression
  library(BLR)
  mydat <- data.frame(y,x)
  posterior <- MCMCregress(y ~ x, data = mydat)
  sposterior = summary(posterior)
  SlopeByonx = sposterior$statistics[2]
  IcByonx = sposterior$statistics[1]
  # -----------------------------------------------------------
  # (4)  Simple linear regression (SLR): x on y
  # -----------------------------------------------------------
  SLRyonx = lm(x ~ y)
  b = as.numeric(SLRyonx$coefficients[2])
  a = as.numeric(SLRyonx$coefficients[1])
  # x=b*y+a -> y = (x-a)/b -> slope = 1/b; 
  SlopeXonY = 1/b
  IcXonY = -a/b
  # -----------------------------------------------------------
  # (5) Bayesian linear regression (BLR): x on y
  # -----------------------------------------------------------
  posteriorXonY <- MCMCregress(x ~ y, data = mydat)
  sposteriorXonY = summary(posteriorXonY)
  bB = sposteriorXonY$statistics[2]
  aB = sposteriorXonY$statistics[1]
  # x=b*y+a -> y = (x-a)/b -> slope = 1/b; 
  SlopeBxony = 1/bB
  IcBxony = -aB/bB
  # -----------------------------------------------------------
  # (6) Geometric mean of regression slopes:
  # -----------------------------------------------------------
  SlopeGeo = sqrt(SlopeYonX*SlopeXonY)
  SlopeGeoB = sqrt(SlopeByonx*SlopeBxony)
  # centroid (Schwerpunkt)
  xc = mean(x); yc = mean(y)
  # line with slope SlopeGeo through centroid:
  # y-yc = SlopeGeo*(x-xc)
  # x = 0 -> y = IcGeo = yc-SlopeGeo*xc
  IcGeo = yc-SlopeGeo*xc
  IcGeoB = yc-SlopeGeoB*xc
  # -----------------------------------------------------------
  # (7) MLE with true lambda (Casalla & Berger, 2002):
  # -----------------------------------------------------------
  lambda = lambdaTrue
  data = data.frame(x,y)
  CM = cov(data) # covariance matrix
  Sxx = CM[1,1]; Syy = CM[2,2]; Sxy = CM[1,2]
  SlopeMLE=(-(Sxx-lambda*Syy)+sqrt((Sxx-lambda*Syy)^2+4*lambda*Sxy^2))/(2*lambda*Sxy)
  IcMLE = yc-SlopeMLE*xc
  # -----------------------------------------------------------
  # (8) MLE: vary lambda around lambdaTrue
  # -----------------------------------------------------------
  lambdaArr = seq(lambdaTrue/10,lambdaTrue*5,lambdaTrue/50); L = length(lambdaArr)
  SlopeMLEArr = numeric(L); IcMLEArr = numeric(L)
  for(j in 1:L) {
     lambda = lambdaArr[j]
     SlopeMLEArr[j] = (-(Sxx-lambda*Syy)+sqrt((Sxx-lambda*Syy)^2+4*lambda*Sxy^2))/(2*lambda*Sxy)
     IcMLEArr[j] = yc-SlopeMLEArr[j]*xc
  }
  # -----------------------------------------------------------
  # (9) Split into groups:
  # -----------------------------------------------------------
  # -----------------------------------------------------------
  # (9a) Split into groups: Wald (1940) 1:1
  # -----------------------------------------------------------
  fs = sort(x,index.return = TRUE)$ix; xs = x[fs]; ys = y[fs] # sort data
  n1 = round(n/2); n2 = n-n1
  xsc1 = mean(xs[1:n1]); ysc1 = mean(ys[1:n1])
  xsc2 = mean(xs[n2:n]); ysc2 = mean(ys[n2:n])
  SlopeWald = (ysc2-ysc1)/(xsc2-xsc1)
  # slope through (xsc1,ysc1): (y-ysc1) = SlopeWald*(x-xsc1) 
  # x = 0 --> IcWald
  IcWald = ysc1-SlopeWald*xsc1
  # -----------------------------------------------------------
  # (9b) Split into groups: Bartlett (1949) 1:1:1
  # -----------------------------------------------------------
  fs = sort(x,index.return = TRUE)$ix; xs = x[fs]; ys = y[fs] # sort data
  n1 = round(n/3); n2 = n-n1
  xsc1 = mean(xs[1:n1]); ysc1 = mean(ys[1:n1])
  xsc2 = mean(xs[n2:n]); ysc2 = mean(ys[n2:n])
  SlopeBartlett = (ysc2-ysc1)/(xsc2-xsc1)
  # slope through (xsc1,ysc1): (y-ysc1) = SlopeBartlett*(x-xsc1) 
  # x = 0 --> IcWald
  IcBartlett = ysc1-SlopeBartlett*xsc1
  # -----------------------------------------------------------
  # (9c) Split into groups: Gibson & Jowett (1957) 1:2:1
  # -----------------------------------------------------------
  fs = sort(x,index.return = TRUE)$ix; xs = x[fs]; ys = y[fs] # sort data
  n1 = round(n/4); n2 = n-n1
  xsc1 = mean(xs[1:n1]); ysc1 = mean(ys[1:n1])
  xsc2 = mean(xs[n2:n]); ysc2 = mean(ys[n2:n])
  SlopeGibson = (ysc2-ysc1)/(xsc2-xsc1)
  # slope through (xsc1,ysc1): (y-ysc1) = SlopeGibson*(x-xsc1) 
  # x = 0 --> IcWald
  IcGibson = ysc1-SlopeGibson*xsc1
  # -----------------------------------------------------------
  # (10) Print model parameters & results:
  # -----------------------------------------------------------
  print(c(SlopeTrue,'SlopeTrue'))
  print(c(round(SlopeYonX,4),'SlopeYonX'))
  print(c(round(SlopeByonx,4),'SlopeByonx'))
  print(c(round(SlopeXonY,4),'SlopeXonY'))
  print(c(round(SlopeBxony,4),'SlopeBxony'))
  print(c(round(SlopeGeo,4),'SlopeGeo'))
  print(c(round(SlopeGeoB,4),'SlopeGeoB'))
  print(c(round(SlopeMLE,4),'SlopeMLE'))
  print(c(round(SlopeWald,4),'SlopeWald'))
  print(c(round(SlopeBartlett,4),'SlopeBartlett'))
  print(c(round(SlopeGibson,4),'SlopeGibson'))
  print(c(IcTrue,'IcTrue'))
  print(c(round(IcYonX,4),'IcYonX'))
  print(c(round(IcByonx,4),'IcByonx'))
  print(c(round(IcXonY,4),'IcXonY'))
  print(c(round(IcBxony,4),'IcBxony'))
  print(c(round(IcGeo,4),'IcGeo'))
  print(c(round(IcGeoB,4),'IcGeoB'))
  print(c(round(IcMLE,4),'IcMLE'))
  print(c(round(IcWald,4),'IcWald'))
  print(c(round(IcBartlett,4),'IcBartlett'))
  print(c(round(IcGibson,4),'IcGibson'))
  print(c(round(lambdaTrue,6),'lambdaTrue'))
  print(c(round(b,5),'b'))
  print(c(round(bB,5),'bB'))
# -----------------------------------------------------------
# (11) Plot:
# -----------------------------------------------------------
library(latex2exp)
sflag = 2
if (sflag == 2) {
  # png('SlopeLambda230206.png',width=16,height=16,units='cm',res=300)
  plot(lambdaArr,SlopeMLEArr,type='l',lwd=4,col='blue',
       xlab=TeX('$\\lambda$'),ylab='Slope',las=1,cex=0.6,cex.lab=1.5,
       ylim=c(10,17))
  abline(v=lambdaTrue,col='black')
  abline(h=SlopeTrue,col='black')
  abline(h=SlopeMLE,col='magenta')
  abline(h=SlopeGeo,col='red')
  abline(h=SlopeYonX,col='grey',lty=3)
  abline(h=SlopeXonY,col='grey',lty=4)
  xt = 0.05
  text(xt,SlopeYonX+0.3,'y on x',col='black',pos=4)
  text(xt,SlopeXonY+0.3,'x on y',col='black',pos=4)
  text(xt,SlopeGeo+0.3,'geometric mean',col='red',pos=4)
  text(0.13,SlopeMLE+0.2,TeX('$MLE\\, (true\\, \\lambda)$'),col='magenta',pos=4)
  text(0.13,16.2,TeX('$MLE\\, (\\lambda)$'),col='blue',pos=4)
  # dev.off()
}
# ----------------------------------------------------------------
# Remarks:
# The Bayesian regressions give estimates that are very close to those from
#   simple linear regression.
# ----------------------------------------------------------------
