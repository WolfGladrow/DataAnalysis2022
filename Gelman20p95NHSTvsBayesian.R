print('file: Gelman20p95NHSTvsBayesian.R')
print(date())
# Gelman20p95: Hypothesis testing vs Bayesian analysis
# ----------------------------------------------------------------------------
sigma = 1    # true standard deviation
y = 1        # single observation
theta0 = 0  # H0: theta0 = 0
p1 = 1-pnorm(y,theta0,sigma); print(c(round(p1,2),'one-sided p-value'))
p2 = 2*p1; print(c(round(p2,2),'two-sided p-value'))
# posterior N(y,1) = N(1,1) -> probability for theta > 0
p3 = 1-pnorm(0,y,sigma); print(c(round(p3,2),'Pr(theta > 0)'))
# ----------------------------------------------------------------------------
library(latex2exp)
sflag = 1
# ----------------------------------------------------------------------------
if (sflag == 1) { #  two-sided z-test
  z = seq(-4,4,0.01); fz = dnorm(z)
  # png('NHSTvsBayes210502zTest.png',width=16,height=16,units='cm',res=300)
  plot(z,fz,type='l',lwd=3,col='black',xlab='z',ylab='f(z)',las=1,cex.lab=1.5,
       xlim=c(-3,3))
  abline(v=y,col='magenta',lty=4)
  abline(v=-y,col='magenta',lty=4)
  z1 = seq(y,4,0.01); F1 = dnorm(z1)
  zp = c(y,y,z1,y); yp = c(0,F1[1],F1,0)
  polygon(zp,yp,col='magenta')
  z2 = seq(-4,-y,0.01); F2 = dnorm(z2)
  zp2 = c(-4,-4,z2,-4); yp2 = c(F2[1],F2,0,0)
  polygon(zp2,yp2,col='magenta')
  p2r = round(p2,2)
  text(1.2,0.35,bquote(~p[2] == .(p2r)),col='magenta',pos=4,cex=1.5)
  # dev.off()
}
# ----------------------------------------------------------------------------
if (sflag == 2) { # likelihood function, posterior
  thetaArr = seq(-4,5,0.01); LF = dnorm(thetaArr,y,sigma)
  # png('NHSTvsBayes210502posterior.png',width=16,height=16,units='cm',res=300)
  plot(thetaArr,LF,type='l',lwd=3,col='black',xlab=TeX('$\\theta$'),
       ylab=NA,las=1,xlim=c(-3,3),cex.lab=1.5)
  title(ylab=TeX('Posterior $(\\theta | y=1,\\sigma = 1)$'),line=2.3,cex.lab=1.5)
  z2 = seq(-4,theta0,0.01); F2 = dnorm(z2,y,sigma)
  zp2 = c(-4,-4,z2,-4); yp2 = c(F2[1],F2,0,0)
  polygon(zp2,yp2,col='magenta')
  z1 = seq(theta0,4,0.01); F1 = dnorm(z1,y,sigma)
  zp = c(theta0,theta0,z1,4,theta0); yp = c(0,F1[1],F1,0,0)
  polygon(zp,yp,col='blue')
  qA = round(p3,2)
  text(1.6,0.35,bquote(~p[B] == .(qA)),col='blue',pos=4,cex=1.5)
  qB = round((1-p3),2)
  text(-1.8,0.35,bquote(~{1 - p[B]} == .(qB)),col='magenta',pos=4,cex=1.5)
  # dev.off()
}
# -----------------------------------------------------------------------------
# Results:
# "file: Gelman20p95NHSTvsBayesian.R"
# "Sat Dec 17 17:40:53 2022"
# "0.16"              "one-sided p-value"
# "0.32"              "two-sided p-value"
# "0.84"              "Pr(theta > 0)"
# -----------------------------------------------------------------------------
