print('file: LS-example1.R')
# generate artificial data & fit straight line; n=30
beta0 = 8; beta = -1.2 # true intercept & slope
sigma = 1;             # true standard deviation of normal noise
# ----------- generate data:
set.seed(1953)   # set seed for random number generators
n = 30           # sample size
dx = 4/(n-1); x = seq(1,5,dx); noise = sigma*rnorm(n)
y = beta0 + beta*x + noise
# ----------- fit straight line:
out1 = lm(y~x); out2 = summary(out1)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)   8.1393     0.4953  16.433 6.52e-16 ***
#  x            -1.2469     0.1534  -8.128 7.54e-09 ***
b0 = out1$coefficients[1]; b = out1$coefficients[2]
ub0 = out2$coefficients[3]; ub = out2$coefficients[4]
res=out1$residuals
resSTD = (res-mean(res))/sd(res)  # standardize residuals
# ----------- plot: 
sflag = 2
if (sflag == 1) { # plot data
  # png('SLFn30data160925.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='black',cex=0.6,ylim=c(0,10),
       xlim=c(0,max(x)),xlab='x',ylab='y',las=1,cex.lab=1.5)
  # dev.off()
}
if (sflag == 2) { # data & fitted straight line
  # png('SLFn30estimates160925.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=3,col='black',cex=0.4,ylim=c(0,10),
       xlim=c(0,max(x)),xlab='x',ylab='y',las=1,cex.lab=1.5)
  xp = c(min(x),max(x))
  yp = out1$coefficients[1]+out1$coefficients[2]*xp
  lines(xp,yp,col='blue',lwd=3)
  xt = 0
  text(xt,6,bquote(~beta[0] == .(beta0)),col='black',pos=4,cex=1.5)
  text(xt,4.5,bquote(~beta == .(beta)),col='black',pos=4,cex=1.5)
  br = round(b,2); ubr = round(ub,2)
  text(xt,0.5,bquote(~hat(beta) == .(br) %+-% .(ubr)),col='blue',pos=4,cex=1.5)
  b0r = round(b0,2); ub0r = round(ub0,2)
  text(xt,2.5,bquote(~hat(beta)[0] == .(b0r) %+-% .(ub0r)),col='blue',pos=4,cex=1.5)
  # dev.off()
}
if (sflag == 3) { # plot residuals over x
  # png('SLFn30resx160925.png',width=16,height=16,units='cm',res=300)
  plot(x,res,type='p',lwd=4,col='blue',cex=0.6,
       xlim=c(0,max(x)),xlab='x',ylab='Residuals',las=1,cex.lab=1.5)
  abline(h=0,col='magenta',lty=2)
  # dev.off()
}
if (sflag == 4) { # histogram of residuals & KS-test
  # png('SLFn30reshist160925.png',width=16,height=16,units='cm',res=300)
  hist(res,col='blue',xlab='Residuals',las=1,main='',cex.lab=1.5)
  out3 = ks.test(resSTD,'pnorm')
  out4 = shapiro.test(resSTD)
  p = out3$p.value
  pS = out4$p.value
  text(-1.5,12,paste('p = ',as.character(round(pS,3))),col='blue',cex=1.5)
  text(-1.5,10,'(Shapiro-Wilk test)',col='blue',cex=1.5)
  text(2,12,paste('p = ',as.character(round(p,3))),col='blue',cex=1.5)
  text(2,10,'(KS test)',col='blue',cex=1.5)
  # dev.off()
}
if (sflag == 5) { # estimate of density of standardized residuals
  # png('SLFn30resdensity160925.png',width=16,height=16,units='cm',res=300)
  plot(density(resSTD,from=-3.5,to=3.5),type='l',lwd=3,col='blue',cex.lab=1.5,
       xlab='Residuals',ylab='Estimated density',main='',las=1,xlim=c(-3,3))
  # dev.off()
}
if (sflag == 6) { # estimate of density of standardized noise
  # png('SLFn30noisedensity160925.png',width=16,height=16,units='cm',res=300)
  noiseSTD = (noise-mean(noise))/sd(noise)
  plot(density(noiseSTD,from=-3.5,to=3.5),type='l',lwd=3,col='blue',cex.lab=1.5,
       xlab='Noise',ylab='Estimated density',main='',las=1,xlim=c(-3,3))
  # dev.off()
}
print('Test for homo- versus heteroskedasticity:')
# install.packages('car')
library(car)
pskedasticity = ncvTest(lm(y ~ x))$p; print(c(round(pskedasticity,4),'p(skedasticity)'))
print('Null hypothesis H0: residuals are homoskedastic')
if (pskedasticity >= 0.05) print('Do not reject H0 on significance level alpha=0.05')
if (pskedasticity < 0.05)  print('Reject H0 on significance level alpha=0.05')