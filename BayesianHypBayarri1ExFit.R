print('file: BayesianHypBayarri1ExFit.R')
# Bayarri et al. (2008): Example 1
x = 0:3         # number of events
y = c(81,9,7,1) # frequency of events
# ---------------------------------------------------
# Fit Poisson distribution to data:
lambdaEst = sum(x*y)/sum(y) # estimate mean rate
yPredict = dpois(x,lambdaEst)*sum(y) # predict frequencies
# png('Bayarri08PoissonFit191114.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='p',lwd=4,col='blue',xlab='Number of events',ylab='Frequency',las=1,cex=0.6,axes=F,cex.lab=1.5)
axis(side=1,labels=x,at=x)
yL = c(0,20,40,60,80)
axis(side=2,labels=yL,at=yL,las=1)
points(x,yPredict,col='red',lwd=4,cex=0.6,pch=24)
# dev.off()
# ---------------------------------------------------