print('file: PointEstVarNormal.R')
# Estimate variance (sigma^2) from random sample of normal distribution
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
n = 5      # sample size
varest = numeric(M)
for(m in 1:M) varest[m]=var(rnorm(n))
# png('MCvarNormal160227.png',width=16,height=12,units='cm',res=300)
hist(varest,30,col='blue',xlab='Estimate of population variance',main='',las=1,cex.lab=1.5)
meanvarest = mean(varest); print(c(round(meanvarest,3),'meanvarest'))  # 1.041  (expected: close to sigma^2 = 1)
varvarest = var(varest); print(c(round(varvarest,3),'varvarest'))    # 0.558  (no expectation)
sdvarest = sd(varest); print(c(round(sdvarest,3),'sdvarest'))      # 0.747  (no expectation)
text(3.5,130,'mean of estimate',col='blue',cex=1.5)
text(3.5,115,paste('= ',as.character(round(meanvarest,3))),col='blue',cex=1.5)
text(3.5,90,'variance of estimate',col='blue',cex=1.5)
text(3.5,75,paste('= ',as.character(round(varvarest,3))),col='blue',cex=1.5)
text(3.5,50,'sd of estimate',col='blue',cex=1.5)
text(3.5,35,paste('= ',as.character(round(sdvarest,3))),col='blue',cex=1.5)
# dev.off()
print(c(round(min(varest),3),'min(varest)'))
print(c(round(max(varest),3),'max(varest)'))
sigma = 1
expectedSD = sigma/sqrt(n); print(c(round(expectedSD,3),'expectedSD')) 