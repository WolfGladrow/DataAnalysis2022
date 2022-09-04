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
# png('Varest3Normal160820.png',width=16,height=16,units='cm',res=300)
hist(varest3,30,col='blue',main='',
     xlab=TeX('$\\, \\, \\hat{\\sigma_3^2} \\, (\\mu \\, known,\\, 1/(n-1))$'),cex.lab=1.5)
meanvarest3 = mean(varest3); print(c(round(meanvarest3,3),'meanvarest3'))
sdvarest3 = sd(varest3); print(c(round(sdvarest3,3),'sdvarest3'))
varvarest3 = var(varest3); print(c(round(varvarest3,3),'varvarest3'))
meanvarest3r = round(meanvarest3,3); varvarest3r = round(varvarest3,3); sdvarest3r = round(sdvarest3,3)
xt = 4
text(xt,1200,bquote(~mean(hat(sigma)[3]^2) == .(meanvarest3r)),col='blue',cex=1.5)
text(xt,900,bquote(~var(hat(sigma)[3]^2) == .(varvarest3r)),col='blue',cex=1.5)
text(xt,600,bquote(~sd(hat(sigma)[3]^2) == .(sdvarest3r)),col='blue',cex=1.5)
abline(v=1,col='black',lty=1)
abline(v=meanvarest3,col='blue',lty=4)
# dev.off()
# -------------------------------------------------------------------------