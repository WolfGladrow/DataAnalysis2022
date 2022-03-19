print('file: ExpFctFit.R')
# fit of exponential function to artificial data set
# y(x) = alpha*exp(beta*x) + noise
# (1) generate data pairs (xk,yk), k=1,2,...,n & plot
set.seed(1953) # set seed for random number generators
n = 30  # number of data pairs
x = seq(1,30)
alpha = 1.2; beta = 0.03; sigma = 0.5
y = alpha*exp(beta*x)+rnorm(n,0,sigma)
sflag = 1
if (sflag == 1) {
  # png('expFitData171202.png',width=16,height=12,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
  # dev.off()
}
# (2) log-transform y-data -> linear model:
#     log(alpha*exp(beta*x)) = log(alpha) + beta*x
yt = log(y)
# (3) apply simple linear regression:
model1 = lm(yt ~ x)
logaEst = model1$coefficients[1]
aEst = exp(logaEst)             # estimate of alpha
bEst = model1$coefficients[2]   # estimate of beta
model2 = summary(model1)
ulogaEst = model2$coefficients[3]   # estimate
logaEstp = logaEst+ulogaEst  # ln(alpha) + 1 sigma
logaEstm = logaEst-ulogaEst  # ln(alpha) - 1 sigma
aEstp = exp(logaEstp)
aEstm = exp(logaEstm)
ubEst = model2$coefficients[4]  # estimate of uncertainty (1-sigma) of beta
# (3) plot fit & data:
yf = aEst*exp(bEst*x)
sflag = 0
if (sflag == 2) {
  # png('expFitMod171202.png',width=16,height=12,units='cm',res=300)
  plot(x,yf,type='l',lwd=3,col='magenta',xlab='x',ylab='y',las=1,cex.lab=1.5)
  points(x,y,col='blue',lwd=4,cex=0.6)
  # dev.off()
}
# (4) plot residuals
yr = model1$residuals
sigmaEst = sd(yr)
sflag = 0
if (sflag == 3) {
  # png('expFitRes171202.png',width=16,height=12,units='cm',res=300)
  plot(x,yr,type='p',lwd=4,col='black',xlab='x',ylab='Residuals',las=1,cex=0.6,cex.lab=1.5)
  abline(0,0,col='green',lty=4)
  # dev.off()
}