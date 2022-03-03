print('file: PointEstPoissonSampleMean.R')
# Poisson: sample mean estimator 
set.seed(1953)
M = 1e4   # number of Monte-Carlo runs
n = 5     # sample size
lambda1 = 2.7  # rate (mean number of events) = variance
muest = numeric(M)
for(m in 1:M) muest[m]=mean(rpois(n,lambda=lambda1))
library(latex2exp)
# png('MCmeanPois161224.png',width=16,height=12,units='cm',res=300)
hist(muest,30,col='blue',xlab=TeX('$\\hat{\\lambda}$'),main='',las=1,cex.lab=1.5)
meanmuest = mean(muest)  # (expected: close to mu = 2.7)
varmuest = var(muest) # (expected: close to sigma^2/n=lambda/n=2.7/5=0.54)
sdmuest = sd(muest)   # (expected: sqrt(lambda/n)=0.73)
text(1.1,1050,expression(paste('mean of ',hat(lambda))),col='blue',cex=1.5)
text(1.1,900,paste(' =',as.character(round(meanmuest,3))),col='blue',cex=1.5)
text(4.5,1050,expression(paste('var. of ',hat(lambda))),col='blue',cex=1.5)
text(4.5,900,paste(' =',as.character(round(varmuest,3))),col='blue',cex=1.5)
text(4.5,650,expression(paste('sd of ',hat(lambda))),col='blue',cex=1.5)
text(4.5,500,paste(' =',as.character(round(sdmuest,3))),col='blue',cex=1.5)
# dev.off()
print(c('sqrt(lambda1/n) = ',sqrt(lambda1/n)))  # 0.7348469