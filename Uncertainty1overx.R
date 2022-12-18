print('file: Uncertainty1overx.R')
# Propagation of variance: 1/x: Monte Carlo & robust estimate
x = 2.1; sx = 0.5
# (a) analytic:
dfdx = -1/x^2
ssqfana = dfdx^2*sx^2      # 
sfana = sqrt(dfdx^2*sx^2); print(c(round(sfana,2),'sfana (analytic)'))  # 0.1133787
f = 1/x; print(c(round(f,2),'f (analytic)'))                    # 0.4761905
# (b) Monte Carlo:
set.seed(1953) # set seed for random number generators
M = 10000      # number of Monte Carlo runs
rx=rnorm(M,mean=x,sd=sx)
rf = 1/rx
sfMC = sd(rf); print(c(round(sfMC,2),'sfMC'))
meanMC = mean(rf); print(c(round(meanMC,2),'meanMC'))
medianMC = median(rf); print(c(round(medianMC,2),'medianMC'))
#  Normalized Median Absolute Deviation (MADN) estimate:
sfMCrobust= median(abs(rf-median(rf)))/0.6745; print(c(round(sfMCrobust,2),'sfMCrobust'))
medianMCr = round(medianMC,2); sfMCrobustr = round(sfMCrobust,2)
sflag = 1
library(latex2exp)
if (sflag == 1) {
  # png('MC1oxHist161203.png',width=16,height=16,units='cm',res=300)
  hist(rf,500,col='blue',main='',las=1,xlim=c(0,2),cex.lab=1.5)
  text(1,700,bquote(~hat(q) == .(medianMCr) %+-% .(sfMCrobustr)),col='blue',cex=1.5)
  text(1,300,paste('M = ',as.character(M)),col='blue',cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  # png('PoV1oxDensity161203.png',width=16,height=16,units='cm',res=300)
  plot(density(rf,from=0.25,to=1.05),type='l',col='blue',main='',las=1,
       xlab='f(rx) = 1/rx',lwd=3,xlim=c(0.3,1),cex.lab=1.5) 
  xp = seq(0,1.2,0.001)
  yp = dnorm(xp,mean=medianMC,sd=sfMCrobust)
  lines(xp,yp,col='black',lty=2,lwd=1)
  legend('topright',legend=c('MC estimate','normal'),col=c('blue','black'),
         lty=c(1,2),lwd=c(3,1),cex=1.3)
  # dev.off()
}
# -----------------------------------------------------------------------------
# Results:
# "file: Uncertainty1overx.R"
# "0.11" "sfana (analytic)"
# "0.48" "f (analytic)"
# "0.2"  "sfMC"
# "0.51" "meanMC"
# "0.48" "medianMC"
# "0.11" "sfMCrobust"
# -----------------------------------------------------------------------------
