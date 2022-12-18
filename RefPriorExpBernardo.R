print('file: RefPriorExpBernardo.R')
print(date())
# Bernardo (2005) numerical reference prior for exponential PDF
M = 500 # number of sample (Monte Carlo)
K = 75  # number of observations per sample
thetaMin = exp(-3); thetaMax = exp(3)
logthetaMin = log(thetaMin); logthetaMax = log(thetaMax)
dlogtheta = 6/8; logthetaA = seq(-3,3,dlogtheta)
thetaA = exp(logthetaA); L = length(thetaA)
set.seed(1953) # set seed for random number generators
rj = numeric(M); priorNN = numeric(L)
for(i in 1:L) {
  for(j in 1:M) {
    theta = thetaA[i]
    x = rexp(K,rate=theta) # K random values from exponential PDF
    xmean = mean(x) # sufficient statistics
    cj = factorial(K)/(K*xmean)^(K+1)
    rj[j] = log(theta^K*exp(-theta*K*xmean)/cj)
  } # end of j-loop (M)
  priorNN[i] = exp(mean(rj))
} # end of i-loop (L)
# rescaling -> pi(1) = 1 
(prior = priorNN/priorNN[5])
print(c(round(prior,4),'prior'))
# difference: num. ref. prior - 1/theta
(Delta = prior - 1/thetaA)
(DeltaRel = 100*(prior - 1/thetaA)/(1/thetaA)) # percent
print(c(round(DeltaRel,4),'DeltaRel'))
library(latex2exp)
xp = seq(0.04,22,0.01); yp = 1/xp
# png('Bernardo05RefPriorExp210609.png',width=16,height=16,units='cm',res=300)
plot(xp,yp,type='l',lwd=1,col='blue',xlab=NA,
       ylab='Prior',las=1,cex.lab=1.5,xlim=c(0,20),ylim=c(0,22))
points(thetaA,prior,col='black',lwd=4,cex=0.6)
title(xlab=TeX('$\\theta$'),cex.lab=1.5)
# dev.off()
# -----------------------------------------------------------------------------
# Results:
# "file: RefPriorExpBernardo.R"
# "Sun Dec 18 19:54:42 2022"
# "20.8311" "9.8651"  "4.5261"  "2.1923"  "1" "0.4909"  "0.2219"  "0.1088" 
# "0.0505"  "prior"  
# "3.7118"   "3.9775"   "0.991"    "3.5561" "0" "3.9143"   "-0.5606" 
# "3.1845"   "1.4234"   "DeltaRel"
# -----------------------------------------------------------------------------
