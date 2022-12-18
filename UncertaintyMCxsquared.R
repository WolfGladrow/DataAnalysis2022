print('file: UncertaintyMCxsquared.R')
# propagation of variances: Monte Carlo, x^2
#      f(x) = x^2; x = 2.1 +- 0.5
x = 2.1; sx = 0.5
# (a) analytic (law of prop. of uncertainty):
f = x^2    # 4.41 
dfdx = 2*x
ssqf = dfdx^2*sx^2
sf = sqrt(ssqf)   # 2.1
# (b) Monte Carlo:
set.seed(1953) # set seed for random number generators
M = 1e4
rx = rnorm(M,mean=x,sd=sx) # random sample from normal PDF
rf = rx^2
meanrf = mean(rf); print(c(round(meanrf,1),'meanrf'))  # 4.63279
sdrf = sd(rf); print(c(round(sdrf,1),'sdrf'))      # standard deviation 2.134695
# robust estimation:
medianMC = median(rf); print(c(round(medianMC,1),'medianMC'))  # 4.407125
medianMCr = round(medianMC,1)
#  Normalized Median Absolute Deviation (MADN) estimate of dispersion:
sfMCrobust= median(abs(rf-median(rf)))/0.6745  # 2.071515 MADN
sfMCrobustr = round(sfMCrobust,1)
print(c(round(sfMCrobust,1),'sfMCrobust'))
library(latex2exp)
sflag = 1
if (sflag == 1) {
  # png('MCxsqHist161203.png',width=16,height=16,units='cm',res=300)
  hist(rf,100,col='blue',main='',xlab='rf',las=1,cex.lab=1.5) #,xlim=c(10,40))
  text(8,350,bquote(~hat(q) == .(medianMCr) %+-% .(sfMCrobustr)),col='blue',pos=4,cex=1.5)
  text(11,150,paste('M = ',as.character(M)),col='blue',cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  # png('MCxsqDensity161203.png',width=16,height=16,units='cm',res=300)
  plot(density(rf,from=0,to=10.5),type='l',col='blue',main='',xlab='rf',
       las=1,lwd=3,xlim=c(0,10),cex.lab=1.5)
  xp = seq(0,20,0.01); yp = dnorm(xp,mean=medianMC,sd=sfMCrobust)
  lines(xp,yp,col='black',lwd=1,lty=2)
  text(5,0.05,bquote(~hat(q) == .(medianMCr) %+-% .(sfMCrobustr)),col='blue',pos=1,cex=1.5)
  text(5,0.005,paste('M = ',as.character(M)),col='blue',cex=1.5)
  legend('topright',legend=c('MC estimate','normal'),col=c('blue','black'),
         lty=c(1,2),lwd=c(3,1),cex=1.3)
   # dev.off()
}
# -----------------------------------------------------------------------------
# Results:
# "file: UncertaintyMCxsquared.R"
# "4.6"  "meanrf"
# "2.1"  "sdrf"
# "4.4"  "medianMC"
# "2.1"  "sfMCrobust"
# -------------------------------------------------------------------

