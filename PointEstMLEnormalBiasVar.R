print('file: PointEstMLEnormalBiasVar.R')
# Maximum Likelihood Estimate for variance of standard normal PDF: bias & variance
set.seed(1953) # set seed for random number generators
n = 5     # sample size
M = 1e4   # number of Monte Carlo runs
ssq = numeric(M)
for(i in 1:M) {x=rnorm(n); xmean=mean(x); ssq[i]=sum((x-xmean)^2)/n}
ssqmean = mean(ssq); print(c(round(ssqmean,4),' ssqmean'))
library(latex2exp)
# png('MLEvarNormal181228.png',width=16,height=16,units='cm',res=300)
hist(ssq,col='blue',breaks=round(sqrt(M)),
     xlab=TeX('$\\hat{\\sigma^2}$'),ylab='Frequency',las=1,main='',cex.lab=1.5)
abline(v=1,col='black')
abline(v=ssqmean,col='blue',lty=4)
text(2,450,TeX('$\\sigma^2 = 1$'),col='black',pos=4,cex=1.5)
text(2,350,TeX('$E(\\hat{\\sigma^2}) = 0.8$'),col='blue',pos=4,cex=1.5)
text(2,250,bquote(~mean(hat(sigma^2)) == .(ssqmeanr)),col='blue',pos=4,cex=1.5)
text(2,150,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
# dev.off()