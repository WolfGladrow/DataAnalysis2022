print('file: BayesianEstNormalMeanVarUnknown.R')
print(date())
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
varPost = ssq*nu/(nu-2); print(c(round(varPost,4),'var of posterior'))
sdPost = sqrt(varPost); print(c(round(sdPost,4),'sd of posterior'))
myqnst = function(p,location,scale,df) {
  # quantiles of non-standardized t PDF; DWG 6/2021
  tstat <- qt(p,df)
  return(tstat * scale + location)
}
alpha = 0.05; pLower = alpha/2; pUpper = 1-alpha/2 # 95% interval
I95L = myqnst(pLower,ymean,SE,nu)
I95U = myqnst(pUpper,ymean,SE,nu)
print(c(round(I95L,4),'I95L'))
print(c(round(I95U,4),'I95U'))
x95 = c(I95L,I95U); y95 = c(0,0)
library(latex2exp)
# png('MargPostNorm5nExample210622.png',width=16,height=16,units='cm',res=300)
plot(xp,yp,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1,
     cex=0.4,xlim=c(3.1,4.1)) # xlim=c(2,5)) before 3/2022
abline(v=mu,col='black')
abline(v=ymean,col='blue',lty=2)
lines(x95,y95,col='magenta',lwd=3)
xt = 3.57
text(3.35,0.3,'95% interval',col='magenta',pos=4,cex=1.5)
text(3.1,3.5,TeX('$\\mu = 3.3$'),col='black',pos=4,cex=1.5)
text(3.1,3,TeX('$\\sigma = 0.5$'),col='black',pos=4,cex=1.5)
text(xt,4,paste('n = ',as.character(n)),col='blue',pos=4,cex=1.5)
text(xt,3.5,paste('sample mean = ',as.character(round(ymean,3))),col='blue',pos=4,cex=1.5)
text(xt,3,paste('sample var. = ',as.character(round(ssq,3))),col='blue',pos=4,cex=1.5)
title(xlab=TeX('Mean $\\mu$'),cex.lab=1.5)
title(ylab=TeX('Marginal posterior $p(\\mu | y)$'),line=2.5,cex.lab=1.5)
# dev.off()
# -----------------------------------------------------------------------------
# Remarks:
# No R function is available for the non-standardized t density 
#    required here. However, this density can be calculated from
#    the t density by (1) changing the test statistic:
#         tstat = (x-location)/scale)
#    and (2) by scaling the t density
#         dt(tstat,df)/scale
#    The function mydnst() (my density of the non-standard t)
#    does the job.
#    The function myqnst() (my quantiles of the non-standard t)
#    provides quantiles for the non-standard t density.
# -----------------------------------------------------------------------------
# Results: 
# "file: BayesianEstNormalMeanVarUnknown.R"
# "Thu Dec 15 22:27:36 2022"
# " ---------------------------------------------------"
# "Calculate sufficient statistics:"
# "3.5036"    "sample mean"
# "0.0343"    "sample variance"
# "0.1851"    "sample sd"
# "0.0828"    "sd1n standard error of the mean"
# " ---------------------------------------------------"
# "Marginal posterior for mu:"
# "0.0685"  "var of posterior"
# "0.2618"  "sd of posterior"
# "3.2738"  "I95L"  
# "3.7335"  "I95U"  
# -----------------------------------------------------------------------------