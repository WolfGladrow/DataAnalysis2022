print('file: RefPriorAsyTri.R')
print(date())
# Berger(2011, Example 11) asysmmetric triangular PDF
# ---------------------------------------------------
# Numerical estimation of reference prior: Monte Carlo
# ---------------------------------------------------
# Select theta values at which the prior should be estimated:
thetaL = c(0.01,0.05,0.1,0.17,0.27,0.37,0.5,0.63,0.73,0.83,0.9,0.95,0.99)
L = length(thetaL)
eps = 0.001 # lower integration limit and resolution
xpi = seq(eps,1-eps,eps) # array for numerical integration
# M = 2500; K = 2000 # values used by Berger et al. (2009): would take to long
M = 500; K = 100 # using small values 
x = numeric(K); p = numeric(K)
# install.packages('triangle')
library(triangle)
set.seed(1953) # set seed for random number generators
Like = matrix(data=NA,nrow=L,ncol=M)
cj = numeric(M); rj = numeric(M); prior = numeric(L)
for(i in 1:L) {
  for(j in 1:M) {
    thetad = thetaL[i] # discrete theta values
    x = rtriangle(K,0,1,thetad) # K random numbers from tri. PDF
    p = dtriangle(x,0,1,thetad) # likelihoods for single data points
    Like[i,j] = prod(p) # joint likelihood
    integrand = function(z,K,x) {      # z = theta
      Lz = length(z); rq = numeric(Lz) # return array
      for(i in 1:Lz) {
        q = 1; zz = z[i]
        for(k in 1:K) {
          q1 = 2*x[k]/zz
          if(x[k] > zz) q1 = 2*(1-x[k])/(1-zz)
          q = q*q1
        }
        rq[i] = q
      }
      return(rq)
    } # end of function
    cj[j] = sum(integrand(xpi,K,x))*eps
    rj[j] = log(Like[i,j]/cj[j])
  } # j-loop (M) Monte Carlo
  prior[i] = exp(mean(rj))
} # i-loop (L) theta(i)
prior = prior/prior[7]*2/pi # rescale: pi(1/2) = 2/pi = beta(0.5,0.5,0.5)
library(latex2exp)
xp = seq(0.001,0.999,0.001); yp = dbeta(xp,0.5,0.5)
# png('Berger09PriorTriangPDF210614.png',width=16,height=16,units='cm',res=300)
plot(thetaL,prior,type='p',lwd=4,col='blue',xlab=NA,
     ylab=NA,las=1,cex=0.6,ylim=c(0,3.5))
lines(xp,yp,col='black',lwd=1)
title(ylab=TeX('Prior $\\pi(\\theta)$'),line=2.5,cex.lab=1.5)
title(xlab=TeX('$\\theta$'),cex.lab=1.5)
# dev.off()
print(date())
# ----------------------------------------------------------------
# Remarks:
# Calculation took less than 2 minutes.
# ----------------------------------------------------------------