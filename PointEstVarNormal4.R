print('file: PointEstVarNormal4.R')
# 4 different estimators for the variance
set.seed(1953)
M = 1e4   # number of Monte-Carlo runs
n = 5      # sample size
mu = 0     # true mean of population
varest1 = numeric(M); varest2 = numeric(M)
varest3 = numeric(M); varest4 = numeric(M)
for(m in 1:M) { r = rnorm(n);  # random sample
varest1[m]=var(r);               # 1/(n-1), mu unknown
varest2[m]=var(r)*(n-1)/n;       # 1/n, mu unknown
varest3[m]=sum((r-mu)^2)/(n-1);  # 1/(n-1), mu known
varest4[m]=sum((r-mu)^2)/n}      # 1/n, mu known
library(latex2exp)
# png('Varest1to4Normal160820.png',width=16,height=12,units='cm',res=300)
par(mfrow=c(2,2))  # allow for 2 x 2 panels in one figure
xt=3.4; yt = 1250
hist(varest1,30,col='blue',ylim=c(0,1500),main='',las=1,
   xlab=TeX('$\\hat{\\sigma^2} \\, (\\mu \\, unknown,\\, 1/(n-1))$'),cex.lab=1.5)
   meanvarest1 = mean(varest1); sdvarest1 = sd(varest1)
text(xt,yt,col='blue',paste(as.character(round(meanvarest1,3)),
         ' \u00B1 ',as.character(round(sdvarest1,3))),cex=1.5)
hist(varest2,30,col='blue',main='',las=1,
   xlab=TeX('$\\hat{\\sigma^2} \\, (\\mu \\, unknown,\\, 1/n)$'),cex.lab=1.5)
   meanvarest2 = mean(varest2); sdvarest2 = sd(varest2)
text(xt,yt,col='blue',paste(as.character(round(meanvarest2,3)),
          ' \u00B1 ',as.character(round(sdvarest2,3))),cex=1.5)
hist(varest3,30,col='blue',ylim=c(0,1500),cex.lab=1.5,
   xlab=TeX('$\\hat{\\sigma^2} \\, (\\mu \\, known,\\, 1/(n-1))$'),main='',las=1)
   meanvarest3 = mean(varest3); sdvarest3 = sd(varest3)
text(xt,yt,col='blue',paste(as.character(round(meanvarest3,3)),
          ' \u00B1 ',as.character(round(sdvarest3,3))),cex=1.5)
hist(varest4,30,col='blue',cex.lab=1.5,
   xlab=TeX('$\\hat{\\sigma^2} \\, (\\mu \\, known,\\, 1/n)$'),main='',las=1)
   meanvarest4 = mean(varest4); sdvarest4 = sd(varest4)
text(xt,yt,col='blue',paste(as.character(round(meanvarest4,3)),
        ' \u00B1 ',as.character(round(sdvarest4,3))),cex=1.5)
# dev.off()
