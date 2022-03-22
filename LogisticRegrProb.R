print('file: LogisticRegrProb.R')
# logistic regression: linear predictor & probability of success
# logit(p) = ln(p/(1-p)) = beta0 + beta*x = eta
# -> p = 1/(1+exp(-eta))
beta0 = 0.2; beta = 3; xmin = -1; xmax = 1; n = 100
dx = (xmax-xmin)/(n-1); x = round(seq(xmin,xmax,dx),3)
eta = beta0+beta*x  # linear predictor
p = 1/(1+exp(-eta)) # probability of success
library(latex2exp)
sflag = 1
if(sflag == 1) {
  # png('LogRegEta161001.png',width=16,height=12,units='cm',res=300)
  plot(x,eta,type='l',lwd=3,col='blue',xlab='x',
       ylab=NA,las=1,cex.lab=1.5)
  title(ylab=TeX('$Linear\\, predictor\\, \\eta (x)$'),line=2.5,cex.lab=1.5)
  # dev.off()
}
if(sflag == 2) {
  # png('LogRegP161001.png',width=16,height=12,units='cm',res=300)
  plot(x,p,type='l',lwd=3,col='blue',xlab='x',ylab=NA,las=1,cex=0.4,cex.lab=1.5)
  title(ylab='Probability of success p',line=2.5,cex.lab=1.5)
  # dev.off()
}
