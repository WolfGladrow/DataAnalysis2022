print('file: LogRegrEx1n100.R')
# apply logistic regression to artificial data
# data: dflag = 1 generate artificial data
#       dflag = 2 read data from file
dflag = 1
if (dflag == 1) { # generate artificial data
beta0 = 0.2; beta = 3; xmin = -1; xmax = 1
n = 100        # larger sample size
dx = (xmax-xmin)/(n-1); x = round(seq(xmin,xmax,dx),3)
logitp = beta0+beta*x
p = 1/(1+exp(-logitp))
set.seed(1953) # set seed for random number generators
y = numeric(n)
for(k in 1:n) y[k] = rbinom(1,1,prob=p[k]) # random sample from binomial distribution
write.table(x,'xLogReg160930n100.txt')
write.table(y,'yLogReg160930n100.txt')
}
if (dflag == 2) { # read data from file
xread = read.table('xLogReg160930n100.txt',header=T); x = xread$x
yread = read.table('yLogReg160930n100.txt',header=T); y = yread$x # note: $x!!!
}
# logistic regression:
out1 = glm(y ~ x,family=binomial(link='logit'))
out2 = summary(out1)
b0 = out2$coefficients[1]; print(c(round(b0,2),'b0'))  # estimate of beta0
b  = out2$coefficients[2]; print(c(round(b,2),'b'))  # estimate of beta
ub0 = out2$coefficients[3]; print(c(round(ub0,2),'ub0')) # uncertainty of b0
ub  = out2$coefficients[4]; print(c(round(ub,2),'ub')) # uncertainty of b
etaest = b0+b*x
muest = 1/(1+exp(-etaest))
sflag = 2
if(sflag == 1) {
  # png('LogResData220410n100.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
  # dev.off()
}
if(sflag == 2) {
# png('LogResDataFit220410n100.png',width=16,height=16,units='cm',res=300)
    plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
    lines(x,muest,col='magenta',lwd=3)
    beta0 = 0.2; beta = 3;
    eta = beta0+beta*x
    mu = 1/(1+exp(-eta))
    lines(x,mu,col='black',lwd=3,lty=3)
    b0r = round(b0,3); ub0r = round(ub0,3); br = round(b,3); ubr = round(ub,3)
    text(-0.85,0.9,bquote(~hat(beta)[0] == .(b0r) %+-% .(ub0r)),col='magenta',pos=4,cex=1.5)
    text(-0.85,0.8,bquote(~hat(beta) == .(br) %+-% .(ubr)),col='magenta',pos=4,cex=1.5)
    text(-0.85,0.6,bquote(~beta[0] == .(beta0)),col='black',pos=4,cex=1.5)
    text(-0.85,0.5,bquote(~beta == .(beta)),col='black',pos=4,cex=1.5)
    # dev.off()
}