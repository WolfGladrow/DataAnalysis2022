print('file: UncertaintyMCtanh.R')
# propagation of variances f(x) = tanh(3*x-6): Monte Carlo & robust estimation
# (1) f(x) = tanh(3*x-6); x = 2.1 +- 0.5
x = 2.1; sx = 0.5
# (a) analytic (law of prop. of uncertainty):
f = tanh(3*x-6)
dfdx = 3*(1-(tanh(3*x-6))^2)
sfana = sqrt(dfdx^2*sx^2)
# (b) Monte Carlo:
set.seed(1953) # set seed for random number generators
M = 1e4
rx=rnorm(M,mean=x,sd=sx)
rf = tanh(3*rx-6)
meanMC = mean(rf)
medianMC = median(rf)
sfMC = sd(rf)         
#  Normalized Median Absolute Deviation (MADN) estimate:
sfMCrobust= median(abs(rf-median(rf)))/0.6745
sflag = 1
if (sflag == 1) {
  # png('MCtanhHist161203.png',width=16,height=16,units='cm',res=300)
  hist(rf,100,col='blue',main='',las=1,cex.lab=1.5)
  text(-1,750,paste('M = ',as.character(M)),col='blue',cex=1.5,pos=4)
  # dev.off()
}
if (sflag == 2) {
  # png('PoVtanhDensity161203.png',width=16,height=16,units='cm',res=300)
  plot(density(rf,from=-1,to=1),type='l',col='blue',main='',las=1,
       xlab='rf = tanh(3*rx-6)',lwd=3,cex.lab=1.5)
  # dev.off()
}