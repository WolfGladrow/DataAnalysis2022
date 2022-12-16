print('file: BayesianEstNormalVarKnown.R')
print(date())
# Bayesian estimation: normal pop., variance known
print(' ---------------------------------------------------')
print('(1) generate artificial data:')
set.seed(1953)
n = 10; print(c(n,'sample size'))
sigma0sq = 4; print(c(sigma0sq,'true variance sigma_0^2 (known)'))
sigma0 = sqrt(sigma0sq)
mu = 3; print(c(mu,'mu true mean'))
y = rnorm(n,mu,sigma0) # n random numbers from normal PDF 
print(' ---------------------------------------------------')
print('(2) calculate sufficient statistics:')
muEst = mean(y); print(c(round(muEst,4),'sample mean'))
ssq = var(y); print(c(round(ssq,4),'sample variance'))
nu = n-1 # degrees of freedom
print(' ---------------------------------------------------')
print('(3) calculate likelihood function:')
LikeFct = function(x1,n,nu,muest,sigma0sq,ssq) {
  (2*pi*sigma0sq)^(-n/2)*exp(-(nu*ssq+n*(x1-muEst)^2)/(2*sigma0sq))
}
dx = 0.001; x = seq(0,6,dx); Lx = length(x) # x = mu
LikeF = LikeFct(x,n,nu,muest,sigma0sq,ssq)
print(' ---------------------------------------------------')
print('(4) normalize likelihood function:')
(qnorm1 = integrate(LikeFct,lower=0,upper=6,n,nu,muest,sigma0sq,ssq)$value)
print(c(qnorm1,'qnorm1 normalization factor: integration'))
(qnorm2 = n^(-1/2)*(2*pi*sigma0sq)^(-(n-1)/2)*exp(-nu*ssq/(2*sigma0sq)))
print(c(qnorm2,'qnorm2 normalization factor: analytical'))
Post = LikeF/qnorm2 # normalization
print('normalized likelihood function = posterior')
print(' ---------------------------------------------------')
print('Analyze posterior: here: mode = mean = median')
mymode = function(x,y) x[which.max(y)] # DWG 6/2021
(mode1 = mymode(x,Post)); print(c(round(mode1,3),'mode of posterior'))
(mean1 = sum(x*Post*dx)); print(c(round(mean1,3),'mean of posterior'))
(varPost = sum((x-muEst)^2*Post*dx))
print(c(round(varPost,3),'varPost variance of posterior'))
print(' 95% interval around muEst:')
SE = sqrt(sigma0sq/n) # standard error of the mean
print(c(round(SE,4),'SE: standard error of the mean'))
VarPost = sigma0sq/n
print(c(round(VarPost,4),'VarPost variance of posterior'))
I95L = muEst-2*SE; print(c(round(I95L,3),'I95L'))  # 95% approx +- 2*sigma
I95U = muEst+2*SE; print(c(round(I95U,3),'I95U'))
print(' ---------------------------------------------------')
print('Plot:')
library(latex2exp)
xp = x; yp = dnorm(xp,mu,sigma0) # distribution from which we sample
xI95 = c(I95L,I95U); yI95 = c(0,0) # 95% interval
# png('PosteriorNormalVarKnown210617.png',width=16,height=16,units='cm',res=300)
plot(x,Post,type='l',lwd=3,col='blue',xlab=TeX('$\\mu,\\, \\hat{\\mu}$'),
     ylab=NA,las=1,cex=0.4,cex.lab=1.5)
title(ylab='PDFs',line=2.5,cex.lab=1.5)
lines(xp,yp,col='black',lty=1)
abline(v=mu,col='black',lty=2)
abline(v=mode1,col='blue',lty=3)
lines(xI95,yI95,col='magenta',lwd=3,lty=1)
text(muEst-0.1,0.05,'95 % interval',col='magenta',cex=1.5)
legend('topright',legend=c('posterior','population'),col=c('blue','black'),
       lty=c(1,1),lwd=c(3,1),cex=1.2)
# dev.off()
# -----------------------------------------------------------------------------
# Remarks:
# Results:
# "file: BayesianEstNormalVarKnown.R"
# "Fri Dec 16 08:40:57 2022"
# " ---------------------------------------------------"
# "(1) generate artificial data:"
# "10"          "sample size"
# "4"            "true variance sigma_0^2 (known)"
# "3"            "mu true mean"
# " ---------------------------------------------------"
# "(2) calculate sufficient statistics:"
# "3.2683"      "sample mean"
# "2.8369"      "sample variance"
# " ---------------------------------------------------"
# "(3) calculate likelihood function:"
# " ---------------------------------------------------"
# "(4) normalize likelihood function:"
# "6.49954651257341e-09" "qnorm1 normalization factor: integration"
# "6.49959815833094e-09" "qnorm2 normalization factor: analytical"
# "normalized likelihood function = posterior"
# " ---------------------------------------------------"
# "Analyze posterior: here: mode = mean = median"
# "3.268"             "mode of posterior"
# "3.268"             "mean of posterior"
# "0.4"               "varPost variance of posterior"
# " 95% interval around muEst:"
# "0.6325"   "SE: standard error of the mean"
# "0.4"      "VarPost variance of posterior"
# "2.003" "I95L" 
# "4.533" "I95U" 
# -----------------------------------------------------------------------------