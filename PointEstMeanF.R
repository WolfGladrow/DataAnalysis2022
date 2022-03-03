print('file: PointEstMeanF.R')
# Estimate mean from random samples of F distribution
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
n = 5      # sample size
nu1 = 15; nu2 = 5  # degrees of freedom
muest = numeric(M)
expectedmean = nu2/(nu2-2); print(c(round(expectedmean,4),'expectedmean'))  # 5/3 = 1.667 for nu2 > 2
expectedvar = 2*nu2^2*(nu1+nu2-2)/(nu1*(nu2-2)^2*(nu2-4))
print(c(round(expectedvar,4),'expectedvar'))
expectedvarofmean = expectedvar/n
print(c(round(expectedvarofmean,4),'expectedvarofmean'))  # 1.333
for(m in 1:M) muest[m]=mean(rf(n,df1=nu1,df2=nu2))
#  png('MCmeanF160228.png',width=16,height=12,units='cm',res=300)
hist(muest,30,col='blue',xlab='Estimate of population mean',main='',las=1,cex.lab=1.5)
meanmuest = mean(muest); print(c(round(meanmuest,4),'meanmuest'))  # (expected: close to mu = 1.667)
varmuest = var(muest); print(c(round(varmuest,4),'varmuest'))    # (expected: close to sigma^2/n = 1.334)
sdmuest  = sd(muest); print(c(round(sdmuest,4),'sdmuest'))     # (expected: close to sigma/sqrt(n) = 1.15)
text(8,320,'mean of estimate',col='blue',cex=1.5)
text(8,280,paste('= ',as.character(round(meanmuest,4))),col='blue',cex=1.5)
text(8,220,'var of estimate',col='blue',cex=1.5)
text(8,180,paste('= ',as.character(round(varmuest,3))),col='blue',cex=1.5)
# dev.off()