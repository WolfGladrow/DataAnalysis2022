print('file: BayesianEstNormalMeanVarUnknown.R')
# analyze marginal posterior for sample from N(mu,sigma^2)
mu = 3.3; sigma = 0.5; ssq = sigma^2; n = 5; nu = n-1
set.seed(2021) # set seed for random number generators 2021
y = rnorm(n,mu,sigma) # random sample from normal population
print(' ---------------------------------------------------')
print('Calculate sufficient statistics:')
ymean = mean(y); print(c(round(ymean,4),'sample mean'))
ssq = var(y); print(c(round(ssq,4),'sample variance'))
sd1 = sqrt(ssq); print(c(round(sd1,4),'sample sd'))
SE = sd1/sqrt(n); print(c(round(SE,4),'sd1n standard error of the mean'))
print(' ---------------------------------------------------')
print('Marginal posterior for mu:')
xp = seq(1,6,0.01)
mydnst = function(x,location,scale,df) {
  # density of non-standardized t PDF
  tstat = (x-location)/scale
  return(dt(tstat,df)/scale)
  # factor 1/scale in density stems from d tstat/dx = 1/scale 
}
yp = mydnst(xp,ymean,SE,nu)
# $\sigma^2 = \dps \beta^2 \frac{\nu}{\nu - 2}$ for $\nu > 2$
varPost = ssq*nu/(nu-2); print(c(round(varPost,4),'var of posterior'))
sdPost = sqrt(varPost); print(c(round(sdPost,4),'sd of posterior'))
myqnst = function(p,location,scale,df,lower.tail,log.p) {
  # quantiles of non-standardized t PDF; DWG 6/2021
  tstat <- qt(p,df,lower.tail=lower.tail,log.p=log.p)
  return(tstat * scale + location)
}
alpha = 0.05; pLower = alpha/2; pUpper = 1-alpha/2 # 95% interval
I95L = myqnst(pLower,ymean,SE,nu,lower.tail=TRUE,log.p=FALSE)
I95U = myqnst(pUpper,ymean,SE,nu,lower.tail=TRUE,log.p=FALSE)
print(c(round(I95L,4),'I95L'))
print(c(round(I95U,4),'I95U'))
x95 = c(I95L,I95U); y95 = c(0,0)
library(latex2exp)
# png('MargPostNorm5nExample210622.png',width=16,height=12,units='cm',res=300)
plot(xp,yp,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1,
     cex=0.4,xlim=c(3.1,4.1)) # xlim=c(2,5)) before 3/2022
abline(v=mu,col='black')
abline(v=ymean,col='blue',lty=2)
lines(x95,y95,col='magenta',lwd=3)
xt = 3.6
text(3.35,0.3,'95% interval',col='magenta',pos=4,cex=1.5)
text(3.1,3.5,TeX('$\\mu = 3.3$'),col='black',pos=4,cex=1.5)
text(3.1,3,TeX('$\\sigma = 0.5$'),col='black',pos=4,cex=1.5)
text(xt,4,paste('n = ',as.character(n)),col='blue',pos=4,cex=1.5)
text(xt,3.5,paste('sample mean = ',as.character(round(ymean,3))),col='blue',pos=4,cex=1.5)
text(xt,3,paste('sample var. = ',as.character(round(ssq,3))),col='blue',pos=4,cex=1.5)
title(xlab=TeX('Mean $\\mu$'),cex.lab=1.5)
title(ylab=TeX('Marginal posterior $p(\\mu | y)$'),line=2.5,cex.lab=1.5)
# dev.off()