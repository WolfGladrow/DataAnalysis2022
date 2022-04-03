print('file: BayesianHyp-t-test-Data.R')
# plot data & mean for Bayesian-t-test and t-test
x = c(1.5,0.3,1.8,-1.4,0.8,3.0,-0.3,0.2,-0.4,1.9,0.0,0.3,-1.0,
      1.2,3.8,0.5,-0.8,2.0,1.1,1.2,-0.4,2.7,0.5,-1.4,1.1)
Thyp = 0 # hypothesized temperature of null hypothesis H0
# png('EqualMeansData200408.png',width=16,height=16,units='cm',res=300)
plot(x,type='p',lwd=4,col='blue',xlab='Data #',
     ylab=NA,las=1,cex=0.6,cex.lab=1.5)
title(ylab=expression(paste('Temperature (',degree,'C)')),cex.lab=1.5,line=2.3)
abline(Thyp,0,col='black',lty=4)
# dev.off()