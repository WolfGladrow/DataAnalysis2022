print('file: PointEstVarNormalA.R')
# 4 different estimators for the variance: A 1/(n-1), mu unknown
set.seed(1953)
M = 1e4   # number of Monte-Carlo runs
n = 5      # sample size
mu = 0     # true mean of population
varest1 = numeric(M); 
for(m in 1:M) { r = rnorm(n);  # random sample
   varest1[m]=var(r)}          # 1/(n-1), mu unknown
library(latex2exp)
# png('Varest1Normal160820.png',width=16,height=12,units='cm',res=300)
hist(varest1,30,col='blue',main='',las=1,
       xlab=TeX('$\\hat{\\sigma^2} \\, (\\mu \\, unknown,\\, 1/(n-1))$'),cex.lab=1.5)
  meanvarest1 = mean(varest1); print(c(round(meanvarest1,3),'meanvarest1'))
  sdvarest1 = sd(varest1); print(c(round(sdvarest1,3),'sdvarest1'))
  varvarest1 = var(varest1); print(c(round(varvarest1,3),'varvarest1'))
  text(3.5,1300,'mean of estimate',col='blue',cex=1.5)
  text(3.5,1150,paste('= ',as.character(round(meanvarest1,3))),col='blue',cex=1.5)
  text(3.5,900,'variance of estimate',col='blue',cex=1.5)
  text(3.5,750,paste('= ',as.character(round(varvarest1,3))),col='blue',cex=1.5)
  text(3.5,500,'sd of estimate',col='blue',cex=1.5)
  text(3.5,350,paste('= ',as.character(round(sdvarest1,3))),col='blue',cex=1.5)
# dev.off()