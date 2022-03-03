print('file: PointEstVarNormalC.R')
# 4 different estimators for the variance: C 1/(n-1), mu known
set.seed(1953)
M = 1e4   # number of Monte-Carlo runs
n = 5      # sample size
mu = 0     # true mean of population
varest3 = numeric(M)
for(m in 1:M) {r = rnorm(n);    # random sample
   varest3[m]=sum((r-mu)^2)/(n-1)}  # 1/(n-1), mu known
library(latex2exp)
#  png('Varest3Normal160820.png',width=16,height=12,units='cm',res=300)
hist(varest3,30,col='blue',main='',
       xlab=TeX('$\\hat{\\sigma^2} \\, (\\mu \\, known,\\, 1/(n-1))$'),cex.lab=1.5)
  meanvarest3 = mean(varest3); print(c(round(meanvarest3,3),'meanvarest3'))
  sdvarest3 = sd(varest3); print(c(round(sdvarest3,3),'sdvarest3'))
  varvarest3 = var(varest3); print(c(round(varvarest3,3),'varvarest3'))
  xt = 4
  text(xt,1150,'mean of estimate',col='blue',cex=1.5)
  text(xt,1000,paste('= ',as.character(round(meanvarest3,3))),col='blue',cex=1.5)
  text(xt,750,'variance of estimate',col='blue',cex=1.5)
  text(xt,600,paste('= ',as.character(round(varvarest3,3))),col='blue',cex=1.5)
  text(xt,350,'sd of estimate',col='blue',cex=1.5)
  text(xt,200,paste('= ',as.character(round(sdvarest3,3))),col='blue',cex=1.5)
# dev.off()