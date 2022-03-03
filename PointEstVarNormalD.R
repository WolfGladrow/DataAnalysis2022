print('file: PointEstVarNormalD.R')
# 4 different estimators for the variance: D 1/n, mu known
set.seed(1953)
M = 1e4   # number of Monte-Carlo runs
n = 5      # sample size
mu = 0     # true mean of population
varest4 = numeric(M)
for(m in 1:M) { r = rnorm(n);   # random sample
   varest4[m]=sum((r-mu)^2)/n}  # 1/n, mu known
library(latex2exp)
# png('Varest4Normal160820.png',width=16,height=12,units='cm',res=300)
hist(varest4,30,col='blue',main='',
       xlab=TeX('$\\hat{\\sigma^2} \\, (\\mu \\, known,\\, 1/n)$'),cex.lab=1.5)
  meanvarest4 = mean(varest4); print(c(round(meanvarest4,3),'meanvarest4'))
  sdvarest4 = sd(varest4); print(c(round(sdvarest4,3),'sdvarest4'))
  varvarest4 = var(varest4); print(c(round(varvarest4,3),'varvarest4'))
  text(3.5,1300,'mean of estimate',col='blue',cex=1.5)
  text(3.5,1150,paste('= ',as.character(round(meanvarest4,3))),col='blue',cex=1.5)
  text(3.5,900,'variance of estimate',col='blue',cex=1.5)
  text(3.5,750,paste('= ',as.character(round(varvarest4,3))),col='blue',cex=1.5)
  text(3.5,500,'sd of estimate',col='blue',cex=1.5)
  text(3.5,350,paste('= ',as.character(round(sdvarest4,3))),col='blue',cex=1.5)
# dev.off()