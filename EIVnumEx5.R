print('file: EIVnumEx5.R')
print(date())
# Errors in variables (EIV): artificial data, large sample size, non-normal
# ----------------------------------------------------------------
# (1) Model parameters and generation of artificial data:
# ----------------------------------------------------------------
set.seed(1953)
SlopeTrue = 15 # true slope (N:P in mol/mol)
IcTrue = -2.5    # true intercept not equal 0 (negative -> at N=0 P is > 0)
n = 1e3    # sample size
sigxError = 0.5
sigyError = 14*sigxError # sigyError/sigxError close, however, not equal to SlopeTrue
# (1a) true P values from a uniform PDF:
xiMean = 1.9    # mean xi concentration
sigxi = 0.7     # standard deviation
xiTrue = runif(n,xiMean-2*sigxi,xiMean+2*sigxi) # rnorm(n,xiMean,sigxi)
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
SlopeMLE = (-(Sxx-lambda*Syy)+sqrt((Sxx-lambda*Syy)^2+4*lambda*Sxy^2))/(2*lambda*Sxy)
IcMLE = mean(y)-SlopeMLE*mean(x)
# ----------------------------------------------------------------
# (3) Simple linear regression (SLR): y on x
# ----------------------------------------------------------------
SLRyonx = lm(y ~ x) # 7.159        9.929 
SlopeYonx = as.numeric(SLRyonx$coefficients[2])
IcYonx = as.numeric(SLRyonx$coefficients[1])
# ----------------------------------------------------------------
# (4) Simple linear regression (SLR): x on y
# ----------------------------------------------------------------
SLRxony = lm(x ~ y) # 0.69541      0.04608
b = as.numeric(SLRxony$coefficients[2])
a = as.numeric(SLRxony$coefficients[1])
# x=b*y+a -> y = (x-a)/b -> slope = 1/b; 
SlopeXony = 1/b
IcXony = -a/b
# ----------------------------------------------------------------
# (5) Plot:
# ----------------------------------------------------------------
xp = c(0,max(x))
ypMLE = SlopeMLE*xp+IcMLE
ypYonx = SlopeYonx*xp+IcYonx
ypXony = SlopeXony*xp+IcXony
yxiTrue = SlopeTrue*xp+IcTrue
# library(latex2exp)
sflag = 1
if (sflag == 1) { # n=1e3 and true lambda gives perfect MLE results, non-normal x
  # png('EIVnutrientMC230127n1000nn.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
  lines(xp,yxiTrue,col='grey',lwd=1,lty=1)
  lines(xp,ypMLE,col='magenta',lwd=3,lty=4)
  lines(xp,ypYonx,col='black',lwd=2,lty=2)
  lines(xp,ypXony,col='black',lwd=2,lty=3)
  # plot(xiTrue,yTrue,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
  # dev.off()
}
# ----------------------------------------------------------------
# (6) Print model parameters & results:
# ----------------------------------------------------------------
print(c(round(SlopeMLE,4),'SlopeMLE'))
print(c(SlopeTrue,'true slope'))
print(c(round(lambdaTrue,6),'lambdaTrue'))
print(c(sigxError,'sigxError'))
print(c(sigyError,'sigyError'))
print(c(round(SlopeYonx,4),'SlopeYonx'))
print(c(round(IcYonx,4),'IcYonx'))
print(c(round(SlopeXony,4),'SlopeXony'))
print(c(round(IcXony,4),'IcXony'))