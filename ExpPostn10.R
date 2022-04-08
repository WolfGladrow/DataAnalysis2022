print('file: ExpPostn10.R')
# Exponential PDF: estimate rate constant n=10
theta0 = 1.3       # true rate constant
n = 10             # sample size
set.seed(1953)     # set seed for random number generators
x = rexp(n,theta0) # random sample from exponential PDF
xmean = mean(x)    # sufficient statistic
posterior = function(theta,n,xmean) {
  return(n^n/factorial(n-1)*xmean^n*theta^(n-1)*exp(-theta*n*xmean))
}
dtheta=0.001; thetaA = seq(0,5,dtheta)
posteriorA = posterior(thetaA,n,xmean)
# ---------------------------------------------------
# Check normalization:
(qnorm = integrate(posterior,lower=0,upper=100,n,xmean)$value)
(1-qnorm) # 2.220446e-16
# ---------------------------------------------------
# Analyze posterior: mean, mode, median, variance, sd:
integrand1 = function(theta,n,xmean) {
  return(theta*posterior(theta,n,xmean))
}
(mean1 = integrate(integrand1,lower=0,upper=100,n,xmean)$value)
(mean2 = sum(thetaA*posterior(thetaA,n,xmean))*dtheta) # pedestrian
(mode1 = thetaA[which.max(posteriorA)])
thetaB = seq(0,10,dtheta)       # larger range for quantile estimation
posteriorB = posterior(thetaB,n,xmean)
Lcdf = length(thetaB)
CDF = numeric(Lcdf)
CDF[1] = posteriorB[1]*dtheta
for(i in 2:Lcdf) CDF[i] = CDF[i-1] + posteriorB[i]*dtheta
(median1 = thetaB[which.min((CDF-0.5)^2)])
(i95L = thetaB[which.min((CDF-0.025)^2)])
(i95U = thetaB[which.min((CDF-0.975)^2)])
integrand2 = function(theta,n,xmean,mean1) {             # variance
  return((theta-mean1)^2*posterior(theta,n,xmean))}
(var1 = integrate(integrand2,lower=0,upper=100,n,xmean,mean1)$value)
(sd1 = sqrt(var1))
sflag = 1
library(latex2exp)
if (sflag == 1) {
  xp = c(i95L,i95U); yp = c(0,0)
  # png('ExpEst10n210613.png',width=16,height=16,units='cm',res=300)
  plot(thetaA,posteriorA,type='l',lwd=4,col='blue',xlab=NA,ylab=NA,las=1)
  abline(v=theta0,col='black',lty=1)
  abline(v=mean1,col='blue',lty=2)
  abline(v=mode1,col='green',lty=3)
  abline(v=median1,col='magenta',lty=4)
  lines(xp,yp,col='magenta',lwd=2)
  title(xlab=TeX('Rate $\\theta$'),cex.lab=1.5)
  title(ylab=TeX('Posterior $p(\\theta | x)$'),line=2.5,cex.lab=1.5)
  text(0,0.45,paste('n = ',as.character(n)),col='blue',pos=4,cex=1.5)
  text(0,0.55,bquote(~theta[0] == .(theta0)),col='black',pos=4,cex=1.5)
  text(3,0.55,paste('mean = ',as.character(round(mean1,2))),col='blue',pos=4,cex=1.5)
  text(3,0.5,paste('median = ',as.character(round(median1,2))),col='magenta',pos=4,cex=1.5)
  text(3,0.45,paste('mode = ',as.character(round(mode1,2))),col='green',pos=4,cex=1.5)
  text(2.3,0.03,'95% interval',col='magenta',pos=4,cex=1.5)
  text(3,0.3,paste('sd = ',as.character(round(sd1,2))),col='blue',pos=4,cex=1.5)
  # dev.off()
}