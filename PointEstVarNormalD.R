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
     xlab=TeX('$\\hat{\\sigma_4^2} \\, (\\mu \\, known,\\, 1/n)$'),cex.lab=1.5)
meanvarest4 = mean(varest4); print(c(round(meanvarest4,3),'meanvarest4'))
sdvarest4 = sd(varest4); print(c(round(sdvarest4,3),'sdvarest4'))
varvarest4 = var(varest4); print(c(round(varvarest4,3),'varvarest4'))
meanvarest4r = round(meanvarest4,3); varvarest4r = round(varvarest4,3); sdvarest4r = round(sdvarest4,3)
xt = 3.5
text(xt,1200,bquote(~mean(hat(sigma)[4]^2) == .(meanvarest4r)),col='blue',cex=1.5)
text(xt,900,bquote(~var(hat(sigma)[4]^2) == .(varvarest4r)),col='blue',cex=1.5)
text(xt,600,bquote(~sd(hat(sigma)[4]^2) == .(sdvarest4r)),col='blue',cex=1.5)
abline(v=1,col='black',lty=1)
abline(v=meanvarest4,col='blue',lty=4)
# dev.off()
# ----------------------------------------------------------------------