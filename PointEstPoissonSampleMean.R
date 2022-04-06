print('file: PointEstPoissonSampleMean.R')
# Poisson: sample mean estimator 
set.seed(1953)
M = 1e4   # number of Monte-Carlo runs
n = 5     # sample size
lambda1 = 2.7  # rate (mean number of events) = variance
muest = numeric(M)
for(m in 1:M) muest[m]=mean(rpois(n,lambda=lambda1))
library(latex2exp)
# png('MCmeanPois161224.png',width=16,height=16,units='cm',res=300)
hist(muest,30,col='blue',xlab=TeX('$\\hat{\\lambda}$'),main='',las=1,cex.lab=1.5)
# hist(muest,30,col='blue',xlab=expression(paste(hat(lambda))),main='',las=1)
meanmuest = mean(muest)  # (expected: close to mu = 2.7)
varmuest = var(muest) # (expected: close to sigma^2/n=lambda/n=2.7/5=0.54)
sdmuest = sd(muest)   # (expected: sqrt(lambda/n)=0.73)
meanmuestr = round(meanmuest,3); varmuestr = round(varmuest,3); sdmuestr = round(sdmuest,3)
xt = 4.5
text(xt,1000,bquote(~mean(hat(lambda)) == .(meanmuestr)),col='blue',cex=1.5)
text(xt,800,bquote(~var(hat(lambda)) == .(varmuestr)),col='blue',cex=1.5)
text(xt,600,bquote(~sd(hat(lambda)) == .(sdmuestr)),col='blue',cex=1.5)
abline(v=lambda1,col='black',lty=1)
abline(v=meanmuest,col='blue',lty=4)
# dev.off()
print(c('sqrt(lambda1/n) = ',sqrt(lambda1/n)))  # 0.7348469