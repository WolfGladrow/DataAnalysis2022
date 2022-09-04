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
# ------------------------------------------------------------------------------------
library(latex2exp)
# png('Varest1to4Normal160820.png',width=16,height=16,units='cm',res=300)
par(mar=c(4.1,4.5,1,1),mfrow=c(2,2))  # mf = multiple frames
xt=2; yt = 1250
hist(varest1,30,col='blue',ylim=c(0,1500),main='',las=1,
     xlab=TeX('$\\hat{\\sigma_1^2} \\, (\\mu \\, unknown,\\, 1/(n-1))$'),cex.lab=1.5)
meanvarest1 = mean(varest1); sdvarest1 = sd(varest1)
meanvarest1r = round(meanvarest1,3); sdvarest1r = round(sdvarest1,3)
text(xt,yt,bquote(.(meanvarest1r) %+-% .(sdvarest1r)),col='blue',pos=4,cex=1.5)
# ------------------------------------------------------------------------------------
hist(varest2,30,col='blue',main='',las=1,
     xlab=TeX('$\\hat{\\sigma_2^2} \\, (\\mu \\, unknown,\\, 1/n)$'),cex.lab=1.5)
meanvarest2 = mean(varest2); sdvarest2 = sd(varest2)
meanvarest2r = round(meanvarest2,3); sdvarest2r = round(sdvarest2,3)
text(xt,yt,bquote(.(meanvarest2r) %+-% .(sdvarest2r)),col='blue',pos=4,cex=1.5)
# ------------------------------------------------------------------------------------
hist(varest3,30,col='blue',ylim=c(0,1500),cex.lab=1.5,
     xlab=TeX('$\\hat{\\sigma_3^2} \\, (\\mu \\, known,\\, 1/(n-1))$'),main='',las=1)
meanvarest3 = mean(varest3); sdvarest3 = sd(varest3)
meanvarest3r = round(meanvarest3,3); sdvarest3r = round(sdvarest3,3)
text(xt,yt,bquote(.(meanvarest3r) %+-% .(sdvarest3r)),col='blue',pos=4,cex=1.5)
# ------------------------------------------------------------------------------------
hist(varest4,30,col='blue',cex.lab=1.5,
     xlab=TeX('$\\hat{\\sigma_4^2} \\, (\\mu \\, known,\\, 1/n)$'),main='',las=1)
meanvarest4 = mean(varest4); sdvarest4 = sd(varest4)
meanvarest4r = round(meanvarest4,3); sdvarest4r = round(sdvarest4,3)
text(xt,yt,bquote(.(meanvarest4r) %+-% .(sdvarest4r)),col='blue',pos=4,cex=1.5)
# dev.off()
# ----------------------------------------------------------------------