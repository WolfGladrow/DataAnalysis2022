print('file: Bisection2504.R')
print(date())
# purpose: compare bisection and geometric lines
# created by: Dieter.Wolf-Gladrow@awi.de 
#    4/2025 version 1.0
# ---------------------------------------------------------
# Small, fixed beta2 and larger, variable beta1: plot resulting
#    slopes: beta betaBisect, betaGeo (for sig(Sxy)=1), betaMean 
# -----------------------------------------------------------
beta2 = 0.2; # small, fixed value
beta1Arr = seq(beta2+0.1,5,0.01) 
gamma1Arr = atan(beta1Arr)       # angle 1
gamma2 = atan(beta2)             # angle 2
gamma3Arr = (gamma1Arr+gamma2)/2 # arithmetic mean of angles 1 and 2 
betaBisectArr = tan(gamma3Arr)   # bisection slope
betaMeanArr = (beta1Arr+beta2)/2 # arithmetic mean of slopes 1 and 2
betaGeoArr = sqrt(beta1Arr*beta2) # geometric slope; sig(Sxy) = 1 assumed
library(latex2exp)
sflag = 1
if (sflag == 1) {
  # png('BisectGeoMean250316.png',width=16,height=16,units='cm',res=300)
  plot(beta1Arr,betaMeanArr,type='l',lwd=4,col='red',
       xlab=NA,ylab=NA,las=1,cex=0.6,cex.lab=1.5,
       ylim=c(0,0.1+max(beta1Arr)/2),lty=2)
  lines(beta1Arr,betaGeoArr,col='blue',lwd=4,lty=1)
  lines(beta1Arr,betaBisectArr,col='magenta',lwd=4,lty=4)
  text(0.2,2.2,TeX('$\\hat{\\beta}_2 = 0.2$'),col='black',pos=4,cex=1.5)
  title(xlab=TeX('$\\hat{\\beta}_1$'),cex.lab=1.5) #,line=2.5)
  title(ylab=TeX('$\\hat{\\beta}_j$'),line=1.9,cex.lab=1.5)
  text(2.3,2,'mean',col='red',pos=4,cex=1.5)
  text(3.5,1.2,'bisection',col='magenta',pos=4,cex=1.5)
  text(1.5,0.4,'geometric',col='blue',pos=4,cex=1.5)
  # dev.off()
}
# ----------------------------------------------------------------
# Remarks:
# ----------------------------------------------------------------
# -------------------------------------------------------------
# sflag = 1
# if (sflag == 1) {
# png('.png',width=16,height=12,units='cm',res=300)
# plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
# dev.off()
# }

# ylab = NA
# title(ylab=expression(paste('Vitamin ',B[12],' (pmol ',L^{-1},')')),line=2)


# jpeg('1710.jpeg',width=16,height=12,units='cm',quality=100,res=300)

# par(mfrow=c(2,2))  # allow for 2 x 2 panels in one plot

# title(ylab=expression(paste(NO[3],' (',mu,'mol ',L^-1,')')),line=2)

# pch=20: bullet,21: filled circle,22: filled square,
# 23: filled diamond,24: filled triangle point-up, 25: filled triangle point down.

# system('pwd')  # Mac print working directory
# getwd()        # PC

# bty='n',col.axis='white',xaxt='n',yaxt='n'  # = Visible off

# text(-2,0.35,expression(paste(hat(mu),' = 0')))
# text(x99,0.1,expression(x[99]),col='red')   # index
# text(xt,55,paste('= ',as.character(round(meansdest,3))),col='blue',pos=4)
# pos=4 -# text to the right of
# \u00B1  # +- symbol
# text(4, 8.4, 'expression(hat(beta) == (X^t * X)^{-1} * X^t * y)',cex = .8)

# This does not work:
# text(xt,55,expression(paste('= ',as.character(round(meansdest,3)))),col='blue')

# xcL = qnorm(alpha/2,mean=mu0,sd=sigma)
# xcU = qnorm(1-alpha/2,mean=mu0,sd=sigma)
# x1=xcU; x2=xmax; y1=dnorm(x1,mean=mu0,sd=sigma); y2=dnorm(x2,mean=mu0,sd=sigma); 
# dx=(x2-x1)/50;
# xn=seq(x1,x2,dx); yn=dnorm(xn,mean=mu0,sd=sigma); xf=c(x2,x1,xn); yf=c(0,0,yn)
# quartz(title='I love my Mac',5,10)
# par(mfrow=c(3,1))
# plot(x,yA,type='l',lwd=4,col='black',xlab='(a)',ylab='Y',las=1,cex=0.6,cex.lab=1.5)
# polygon(xf,yf,col='blue')

# write.table(X1,file='xyz.txt')
# out = read.table('xyz.txt',header=TRUE)
# X1 = out$x

# install.packages('latex2exp') # do only once on your computer
# library(latex2exp)
# title(ylab=TeX('$p_{\\mu_1,\\mu_2}(x)$'),line=2.5)
# text(-3,0.36,bquote(~gamma[1] == .(gamma1) %+-% .(qr)),col='blue',cex=1.5,pos=4)

#  Q = read.table('.txt',header=TRUE) # Q = read.csv('.csv',header=TRUE)