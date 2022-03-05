print('file: PointEstMeanUncertaintyTheorem.R')
# Application of Casella & Berger (2002, p.~213-214, Theorem 5.2.6)')
narr = seq(5,100,5); Ln = length(narr)
mu = 0.5; sigma = 1; uncertain = sigma/sqrt(narr)
xpmu = c(min(narr),max(narr)); ypmu = c(mu,mu)
ypplus = uncertain+mu; ypminus = -uncertain+mu
# ------ Monte Carlo sampling:
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
meanest = numeric(M); estmean = numeric(Ln); estsd = numeric(Ln)
for(k in 1:Ln) {n = narr[k];     # sample size
for(m in 1:M) meanest[m]=mean(rnorm(n,mean=mu));
estmean[k] = mean(meanest);
estsd[k] = sd(meanest)}
# png('MeanPlusMinusUncert160819.png',width=16,height=12,units='cm',res=300)
plot(xpmu,ypmu,type='l',lwd=2,col='blue',xlab='n',las=1,
     ylab='Mean \u00B1 uncertainty',ylim=c(0,mu+sigma/2),cex.lab=1.5)
lines(narr,ypplus,col='blue',lwd=2,lty=2)
lines(narr,ypminus,col='blue',lwd=2,lty=2)
points(narr,estmean,col='red',cex=0.6,lwd=2,pch=19)
points(narr,estmean+estsd,col='red',cex=0.6,lwd=2,pch=25)
points(narr,estmean-estsd,col='red',cex=0.6,lwd=2,pch=25)
# dev.off()