print('file: CollinearityExample.R')
# Collinearity: example with two correlated samples
set.seed(1953) # set seed for random number generators
n = 10  # sample size
X1 = runif(n)
# unit length scaling:
x1 = (X1-mean(X1))/sqrt(sum( (X1-mean(X1))^2)) # sum(x1^2) = 1
X2 = x1+0.05*rnorm(n)
x2 = (X2-mean(X2))/sqrt(sum( (X2-mean(X2))^2))
r12=cor(x1,x2)
beta1 = 1; beta2 = -0.5  # set true beta
Y = beta1*x1+beta2*x2-0.03*rnorm(n)  # add normal noise
y = (Y-mean(Y))/sqrt(sum( (Y-mean(Y))^2))
# ----------------------
# solve linear system of least squares solution:
X = matrix(data=c(x1,x2),nrow=n,ncol=2)
XX = t(X)%*%X
A = solve(XX)  # inverse of XX
rhs = t(X)%*%y
betaEst1 = t(A)%*%rhs
betaEst2 = c(0,0)   # dummy values
betaEst2[1] = (rhs[1]-r12*rhs[2])/(1-r12^2)
betaEst2[2] = (rhs[2]-r12*rhs[1])/(1-r12^2)
# ----------------------
# linear regression
outLR = lm(y ~ x1+x2)
# calculate and plot predicted response:
ypred1 = beta1*x1+beta2*x2              # based on true beta
ypred2 = betaEst1[1]*x1+betaEst1[2]*x2  # based on estimated beta
xp = c(min(y),max(y))*1.2
sflag = 1
if (sflag == 1) {
  # png('Collinearity2Data170612.png',width=16,height=16,units='cm',res=300)
  par(mar=c(4.1,4.5,1,1),mfrow=c(2,2))
  plot(x1,x2,type='p',lwd=4,col='blue',xlab=expression(x[1]),
       ylab=expression(x[2]),las=1,cex=0.6,cex.lab=1.5)
  plot(x1,y,type='p',lwd=4,col='blue',xlab=expression(x[1]),ylab='y',las=1,
       cex=0.6,cex.lab=1.5)
  plot(x2,y,type='p',lwd=4,col='blue',xlab=expression(x[2]),ylab='y',las=1,
       cex=0.6,cex.lab=1.5)
  plot(xp,xp,type='l',lwd=4,col='green',xlab='y',ylab='y predicted',las=1,
       cex=0.6,cex.lab=1.5)
  points(y,ypred1,col='blue',lwd=4,cex=0.6,pch=20)
  points(y,ypred2,col='black',lwd=4,cex=0.6,pch=23)
  # dev.off()
}