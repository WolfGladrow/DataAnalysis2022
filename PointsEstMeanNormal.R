print('file: PointEstMeanNormal.R')
# Estimate mean (mu) from random sample of normal distribution
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
n = 5      # sample size
muest = numeric(M)
for(m in 1:M) muest[m]=mean(rnorm(n))
# png('MCmeanNormal160227.png',width=16,height=12,units='cm',res=300)
hist(muest,30,col='blue',xlab='Estimate of population mean',main='',las=1,cex.lab=1.5)
meanmuest = mean(muest); print(c(round(meanmuest,4),'meanmuest'))  # -0.02700781 (expected: close to mu = 0)
varmuest = var(muest); print(c(round(varmuest,4),'varmuest'))  #  0.1857987 (expected: close to sigma^2/n = 1/5 = 0.2)
sdmuest = sd(muest); print(c(round(sdmuest,4),'sdmuest'))      # 0.4310438
text(-0.75,90,'mean of estimate',col='blue',cex=1.5)
text(-0.8,80,paste('= ',as.character(round(meanmuest,3))),col='blue',cex=1.5)
text(0.8,90,'sd of estimate',col='blue',cex=1.5)
text(0.8,80,paste('= ',as.character(round(sdmuest,3))),col='blue',cex=1.5)
# dev.off()