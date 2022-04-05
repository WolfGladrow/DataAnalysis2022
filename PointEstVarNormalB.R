print('file: PointEstVarNormalB.R')
# 4 different estimators for the variance: B 1/n, mu unknown
set.seed(1953)
M = 1e4   # number of Monte-Carlo runs
n = 5      # sample size
mu = 0     # true mean of population
varest2 = numeric(M)
for(m in 1:M) { r = rnorm(n);  # random sample
   varest2[m]=var(r)*(n-1)/n}  # 1/n, mu unknown
library(latex2exp)
# png('Varest2Normal160820.png',width=16,height=16,units='cm',res=300)
hist(varest2,30,col='blue',main='',
     xlab=TeX('$\\hat{\\sigma^2} \\, (\\mu \\, unknown,\\, 1/n)$'),cex.lab=1.5)
# xlab=expression(paste(hat(sigma)^2,' (',mu,' unknown, 1/n)')),main='',las=1)
meanvarest2 = mean(varest2); print(c(round(meanvarest2,3),'meanvarest2'))
sdvarest2 = sd(varest2); print(c(round(sdvarest2,3),'sdvarest2'))
varvarest2 = var(varest2); print(c(round(varvarest2,3),'varvarest2'))
meanvarest2r = round(meanvarest2,3); varvarest2r = round(varvarest2,3); sdvarest2r = round(sdvarest2,3)
xt = 3
text(xt,1300,bquote(~mean(hat(sigma^2)[a]) == .(meanvarest2r)),col='blue',cex=1.5)
text(xt,1000,bquote(~var(hat(sigma^2)[a]) == .(varvarest2r)),col='blue',cex=1.5)
text(xt,700,bquote(~sd(hat(sigma^2)[a]) == .(sdvarest2r)),col='blue',cex=1.5)
abline(v=1,col='black',lty=1)
abline(v=meanvarest2,col='blue',lty=4)
# dev.off()