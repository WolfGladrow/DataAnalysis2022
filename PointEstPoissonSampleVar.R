print('file: PointEstPoissonSampleVar.R')
# Poisson: estimate rate parameter lambda by sample variance
set.seed(1953)
M = 1e4   # number of Monte-Carlo runs
n = 5      # sample size
lambda1 = 2.7  # rate (mean number of events) = variance
varest = numeric(M)
for(m in 1:M) varest[m]=var(rpois(n,lambda=lambda1))
library(latex2exp)
# png('MCvarPois161224.png',width=16,height=16,units='cm',res=300)
hist(varest,30,col='blue',xlab=TeX('$\\hat{\\lambda}$'),main='',las=1,cex.lab=1.5)
meanvarest = mean(varest); print(c(round(meanvarest,5),'meanvarest'))  # expected: close to mu = 2.7
varvarest = var(varest); print(c(round(varvarest,2),'varvarest'))    # no expectation
sdvarest = sd(varest); print(c(round(sdvarest,2),'sdvarest'))
meanvarestr = round(meanvarest,5); varvarestr = round(varvarest,3); sdvarestr = round(sdvarest,3)
xt = 10
text(xt,1400,bquote(~mean(hat(lambda)) == .(meanvarestr)),col='blue',cex=1.5)
text(xt,1100,bquote(~var(hat(lambda)) == .(varvarestr)),col='blue',cex=1.5)
text(xt,800,bquote(~sd(hat(lambda)) == .(sdvarestr)),col='blue',cex=1.5)
abline(v=lambda1,col='black',lty=1)
abline(v=meanvarest,col='blue',lty=4)
# dev.off()