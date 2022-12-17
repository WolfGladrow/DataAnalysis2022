print('file: LikelihoodFctPoisson10n.R')
print(date())
# Likelihood function, Poisson PD, n=10
set.seed(1953)
lambda0 = 2.73; print(c(lambda0,' lambda0'))  # exact rate parameter lambda of Poisson distribution
n = 10    # sample size
ka = rpois(n,lambda0); print(c(ka,' ka')) # generate random sample from Poisson PD
# likelihood function:
lambdaa = seq(0,5,0.01)  # range of lambda values (plot)
L = dpois(ka[1],lambdaa)
for(i in 2:n) L=L*dpois(ka[i],lambdaa)  # product
jmax = which.max(L)
lambdaEst = lambdaa[jmax]; print(c(lambdaEst,' lambdaEst'))
library(latex2exp)
# png('Like10Pois181208.png',width=16,height=16,units='cm',res=300)
plot(lambdaa,L*1e8,type='l',lwd=3,col='blue',xlab=expression(lambda),cex.lab=1.5,
     ylab=NA,las=1,cex=0.4)
title(ylab=TeX('$Likelihood\\, function\\, \\cdot 10^8$'),line=2.3,cex.lab=1.5)
abline(v=lambda0,col='black',lty=4)
abline(v=lambdaEst,col='blue',lty=2)
text(1.1,3,bquote(~hat(lambda) == .(lambdaEst)),col='blue',pos=4,cex=1.5)
text(3.1,3,bquote(~lambda == .(lambda0)),col='black',pos=4,cex=1.5)
# dev.off()
# ----------------------------------------------------------------
# Results:
# "file: LikelihoodFctPoisson10n.R"
# "Sat Dec 17 19:51:42 2022"
# "2.73"     " lambda0"
# "3"   "2"   "1"   "2"   "3"   "1"   "3"   "3"   "3"   "6"   " ka"
# "2.7"        " lambdaEst"
# ----------------------------------------------------------------
