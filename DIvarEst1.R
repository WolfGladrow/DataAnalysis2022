# print('file: DIvarEst1.R')
# print(date())
# print('4 = variance estimators: varEst1 (8/2022)')
# print('file: PointEstVarNormalA.R')
# 4 different estimators for the variance; varEst1: 1/(n-1), mu unknown
set.seed(1953)
M = 1e4    # number of Monte-Carlo runs
n = 5      # sample size
mu = 3.1   # true mean of population
sigma = 1  # true standard deviation
varest1 = numeric(M); varest2 = numeric(M); 
varest3 = numeric(M); varest4 = numeric(M);
for(m in 1:M) {x = rnorm(n,mu,sigma); xmean = mean(x) # random sample
# varest1[m]=var(r)          # 1/(n-1), mu unknown
varest1[m] = sum((x-xmean)^2)/(n-1)
varest2[m] = sum((x-xmean)^2)/n
varest3[m] = sum((x-mu)^2)/(n-1)
varest4[m] = sum((x-mu)^2)/n
}
library(latex2exp)
k = 1
if (k==1) {
  # png('Varest1Normal160820.png',width=16,height=16,units='cm',res=300)
  hist(varest1,30,col='blue',main='',las=1,
       xlab=TeX('$\\hat{\\sigma^2_1} \\, (\\mu \\, unknown,\\, 1/(n-1))$'),cex.lab=1.5)
  meanvarest1 = mean(varest1); print(c(round(meanvarest1,3),'meanvarest1'))
  sdvarest1 = sd(varest1); print(c(round(sdvarest1,3),'sdvarest1'))
  varvarest1 = var(varest1); print(c(round(varvarest1,3),'varvarest1'))
  meanvarest1r = round(meanvarest1,3); varvarest1r = round(varvarest1,3); sdvarest1r = round(sdvarest1,3)
  xt = 3.5
  text(xt,1300,bquote(~mean(hat(sigma)[1]^2) == .(meanvarest1r)),col='blue',cex=1.5)
  # text(xt,1300,bquote(~mean(hat(sigma^2)[1]) == .(meanvarest1r)),col='blue',cex=1.5)
  text(xt,1000,bquote(~var(hat(sigma)[1]^2) == .(varvarest1r)),col='blue',cex=1.5)
  text(xt,700,bquote(~sd(hat(sigma)[1]^2) == .(sdvarest1r)),col='blue',cex=1.5)
  abline(v=1,col='black',lty=1)
  abline(v=meanvarest1,col='blue',lty=4)
  # dev.off()
} # k=1