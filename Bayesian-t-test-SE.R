print('file: Bayesian-t-test-SE.R')
# plot data & mu0, SE, 1-sample-t-test 
x = c(1.5,0.3,1.8,-1.4,0.8,3.0,-0.3,0.2,-0.4,1.9,0.0,0.3,-1.0,
      1.2,3.8,0.5,-0.8,2.0,1.1,1.2,-0.4,2.7,0.5,-1.4,1.1)
T0 = 0   # hypothesized temperature of null hypothesis H0
xmean = mean(x); print(c(round(xmean,2),'sample mean (deg.C)'))
n = length(x); print(c(n,'sample size n'))   # sample size
SE = sd(x)/sqrt(n)
print(c(round(sd(x),2),'s = sd(x) (deg.C)'))
print(c(round(var(x),2),'s = var(x) (deg.C)^2'))
print(c(round(SE,4),'standard error of the mean (deg.C)'))
dis = (xmean-T0)/SE; print(c(round(dis,2),'t = (xmean-mu0)/SE'))
tvalue = (xmean-T0)/SE; print(c(round(tvalue,3),'tvalue (pedestrian)'))
tobs = as.numeric(t.test(x,mu=T0)$statistic); print(c(round(tobs,3),'tvalue = tobs (t-test)'))
# png('EqualMeansDataSE200408.png',width=16,height=16,units='cm',res=300)
plot(x,type='p',lwd=4,col='blue',xlab='Data #',ylab=NA,las=1,cex=0.6,cex.lab=1.5)
title(ylab=expression(paste('Temperature (',degree,'C)')),cex.lab=1.5,line=2.3)
abline(mean(x),0,col='blue',lty=1)
abline(mean(x)+SE,0,col='red',lty=2)
abline(mean(x)-SE,0,col='red',lty=2)
abline(T0,0,col='black',lty=4)
# dev.off()