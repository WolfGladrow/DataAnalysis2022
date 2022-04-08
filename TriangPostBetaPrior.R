print('file: TriangPostBetaPrior.R')
# posterior for samples from triangular PDF based on beta prior
library(triangle)
library(latex2exp)
thetaMode = 0.1 # true mode
a = 1
if(a == 1) { # Berger09TriangPost30M210615
  # png('Berger09TriangPost30M210615.png',width=16,height=16,units='cm',res=300)
  set.seed(1953) # set seed for random number generators
  M = 30
  x = rtriangle(M,0,1,thetaMode)
  dtheta = 0.001; thetaA = seq(dtheta,1-dtheta,dtheta); L = length(thetaA)
  Like = numeric(L)
  for(i in 1:L) {
    theta = thetaA[i]
    p = dtriangle(x,0,1,theta) # likelihoods for single data points
    Like[i] = prod(p)          # joint likelihood L(x | theta)
  }
  BetaPrior = dbeta(thetaA,0.5,0.5)
  Post = Like*BetaPrior
  # normalize:
  qnorm = sum(Post)*dtheta
  Post = Post/qnorm
  (mean1 = sum(thetaA*Post)*dtheta)
  (var1 = sum((thetaA-mean1)^2*Post)*dtheta)
  (sd1 = sqrt(var1))
  (mode1 = thetaA[which.max(Post)])
  Lcdf = length(thetaA)
  CDF = numeric(Lcdf)
  CDF[1] = Post[1]*dtheta
  for(i in 2:Lcdf) CDF[i] = CDF[i-1] + Post[i]*dtheta
  (median1 = thetaA[which.min((CDF-0.5)^2)])
  (i95L = thetaA[which.min((CDF-0.025)^2)])
  (i95U = thetaA[which.min((CDF-0.975)^2)])
  xp = c(i95L,i95U); yp = c(0,0)
  plot(thetaA,Post,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1,cex=0.4,cex.lab=1.5)
  abline(v=thetaMode,col='black')
  abline(v=mean1,col='blue',lty=2)
  abline(v=mode1,col='green',lty=3)
  abline(v=median1,col='magenta',lty=4)
  lines(xp,yp,col='magenta',lwd=2)
  title(xlab=TeX('$\\theta$'),cex.lab=1.5)
  title(ylab=TeX('Posterior $p(\\theta | x)$'),line=2.3,cex.lab=1.5)
  text(0.4,6,TeX('$\\beta (0.5,0.5)\\, prior$'),col='black',pos=4,cex=1.5)
  text(0.4,5.2,bquote(~theta[mode] == .(thetaMode)),col='black',pos=4,cex=1.5)
  text(0.4,4.2,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
  text(0.4,3.4,paste('mean = ',as.character(round(mean1,4))),col='blue',pos=4,cex=1.5)
  text(0.4,2.6,paste('median = ',as.character(round(median1,4))),col='magenta',pos=4,cex=1.5)
  text(0.4,1.8,paste('mode = ',as.character(round(mode1,4))),col='green',pos=4,cex=1.5)
  text(0.4,1,paste('sd = ',as.character(round(sd1,4))),col='blue',pos=4,cex=1.5)
  text(0.02,0.4,'95% interval',col='magenta',pos=4,cex=1.5)
} # end a == 1
if(a == 2) {
  # png('Berger09TriangPost100M210615.png',width=16,height=16,units='cm',res=300)
  set.seed(1953) # set seed for random number generators
  M = 100
  x = rtriangle(M,0,1,thetaMode)
  dtheta = 0.001; thetaA = seq(dtheta,1-dtheta,dtheta); L = length(thetaA)
  Like = numeric(L)
  for(i in 1:L) {
    theta = thetaA[i]
    p = dtriangle(x,0,1,theta) # likelihoods for single data points
    Like[i] = prod(p)          # joint likelihood L(x | theta)
  }
  BetaPrior = dbeta(thetaA,0.5,0.5)
  Post = Like*BetaPrior
  # normalize:
  qnorm = sum(Post)*dtheta
  Post = Post/qnorm
  (mean1 = sum(thetaA*Post)*dtheta)
  (var1 = sum((thetaA-mean1)^2*Post)*dtheta)
  (sd1 = sqrt(var1))
  (mode1 = thetaA[which.max(Post)])
  Lcdf = length(thetaA)
  CDF = numeric(Lcdf)
  CDF[1] = Post[1]*dtheta
  for(i in 2:Lcdf) CDF[i] = CDF[i-1] + Post[i]*dtheta
  (median1 = thetaA[which.min((CDF-0.5)^2)])
  (i95L = thetaA[which.min((CDF-0.025)^2)])
  (i95U = thetaA[which.min((CDF-0.975)^2)])
  xp = c(i95L,i95U); yp = c(0,0)
  plot(thetaA,Post,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1)
  abline(v=thetaMode,col='black')
  abline(v=mean1,col='blue',lty=2)
  abline(v=mode1,col='green',lty=3)
  abline(v=median1,col='magenta',lty=4)
  lines(xp,yp,col='magenta',lwd=2)
  title(xlab=TeX('$\\theta$'),cex.lab=1.5)
  title(ylab=TeX('Posterior $p(\\theta | x)$'),line=2.3,cex.lab=1.5)
  text(0.4,15,TeX('$\\beta (0.5,0.5)\\, prior$'),col='black',pos=4,cex=1.5)
  text(0.4,13,bquote(~theta[mode] == .(thetaMode)),col='black',pos=4,cex=1.5)
  text(0.4,10,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
  text(0.4,8,paste('mean = ',as.character(round(mean1,4))),col='blue',pos=4,cex=1.5)
  text(0.4,6,paste('median = ',as.character(round(median1,4))),col='magenta',pos=4,cex=1.5)
  text(0.4,4,paste('mode = ',as.character(round(mode1,4))),col='green',pos=4,cex=1.5)
  text(0.4,2,paste('sd = ',as.character(round(sd1,4))),col='blue',pos=4,cex=1.5)
  text(0.2,0.4,'95% interval',col='magenta',pos=4,cex=1.5)
}
if(a == 3) {
  # png('Berger09TriangPostFlat30M210615.png',width=16,height=16,units='cm',res=300)
  set.seed(1953) # set seed for random number generators
  M = 30
  thetaMode = 0.1 # true mode
  x = rtriangle(M,0,1,thetaMode)
  dtheta = 0.001; thetaA = seq(dtheta,1-dtheta,dtheta); L = length(thetaA)
  Like = numeric(L) # matrix(data=NA,nrow=L,ncol=M)
  for(i in 1:L) {
    theta = thetaA[i]
    p = dtriangle(x,0,1,theta) # likelihoods for single data points
    Like[i] = prod(p) # joint likelihood L(x | theta)
  }
  Post = Like # flat prior
  # normalize:
  qnorm = sum(Post)*dtheta
  Post = Post/qnorm
  (mean1 = sum(thetaA*Post)*dtheta)
  (var1 = sum((thetaA-mean1)^2*Post)*dtheta)
  (sd1 = sqrt(var1))
  (mode1 = thetaA[which.max(Post)])
  Lcdf = length(thetaA)
  CDF = numeric(Lcdf)
  CDF[1] = Post[1]*dtheta
  for(i in 2:Lcdf) CDF[i] = CDF[i-1] + Post[i]*dtheta
  (median1 = thetaA[which.min((CDF-0.5)^2)])
  (i95L = thetaA[which.min((CDF-0.025)^2)])
  (i95U = thetaA[which.min((CDF-0.975)^2)])
  xp = c(i95L,i95U); yp = c(0,0)
  plot(thetaA,Post,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1,ylim=c(0,6))
  abline(v=thetaMode,col='black')
  abline(v=mean1,col='blue',lty=2)
  abline(v=mode1,col='green',lty=3)
  abline(v=median1,col='magenta',lty=4)
  lines(xp,yp,col='magenta',lwd=2)
  title(xlab=TeX('$\\theta$'),cex.lab=1.5)
  title(ylab=TeX('Posterior $p(\\theta | x)$'),line=2.3,cex.lab=1.5)
  text(0.4,6,'flat prior',col='black',pos=4,cex=1.5)
  text(0.4,5.2,bquote(~theta[mode] == .(thetaMode)),col='black',pos=4,cex=1.5)
  text(0.4,4.2,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
  text(0.4,3.4,paste('mean = ',as.character(round(mean1,4))),col='blue',pos=4,cex=1.5)
  text(0.4,2.6,paste('median = ',as.character(round(median1,4))),col='magenta',pos=4,cex=1.5)
  text(0.4,1.8,paste('mode = ',as.character(round(mode1,4))),col='green',pos=4,cex=1.5)
  text(0.4,1,paste('sd = ',as.character(round(sd1,4))),col='blue',pos=4,cex=1.5)
  text(0.02,0.4,'95% interval',col='magenta',pos=4,cex=1.5)
} # end a == 3
if(a == 4) {
  # png('Berger09TriangPostFlat100M210615.png',width=16,height=16,units='cm',res=300)
  set.seed(1953) # set seed for random number generators
  M = 100
  thetaMode = 0.1 # true mode
  x = rtriangle(M,0,1,thetaMode)
  dtheta = 0.001; thetaA = seq(dtheta,1-dtheta,dtheta); L = length(thetaA)
  Like = numeric(L) # matrix(data=NA,nrow=L,ncol=M)
  for(i in 1:L) {
    theta = thetaA[i]
    p = dtriangle(x,0,1,theta) # likelihoods for single data points
    Like[i] = prod(p) # joint likelihood L(x | theta)
  }
  Post = Like # flat prior
  # normalize:
  qnorm = sum(Post)*dtheta
  Post = Post/qnorm
  (mean1 = sum(thetaA*Post)*dtheta)
  (var1 = sum((thetaA-mean1)^2*Post)*dtheta)
  (sd1 = sqrt(var1))
  (mode1 = thetaA[which.max(Post)])
  # thetaB = seq(0,10,dtheta) # larger range for quantile estimation
  # posteriorB = posterior(thetaB,n,xmean)
  Lcdf = length(thetaA)
  CDF = numeric(Lcdf)
  CDF[1] = Post[1]*dtheta
  for(i in 2:Lcdf) CDF[i] = CDF[i-1] + Post[i]*dtheta
  (median1 = thetaA[which.min((CDF-0.5)^2)])
  (i95L = thetaA[which.min((CDF-0.025)^2)])
  (i95U = thetaA[which.min((CDF-0.975)^2)])
  xp = c(i95L,i95U); yp = c(0,0)
  plot(thetaA,Post,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1)
  abline(v=thetaMode,col='black')
  abline(v=mean1,col='blue',lty=2)
  abline(v=mode1,col='green',lty=3)
  abline(v=median1,col='magenta',lty=4)
  lines(xp,yp,col='magenta',lwd=2)
  text(0.4,15,'flat prior',col='black',pos=4,cex=1.5)
  text(0.4,13,bquote(~theta[mode] == .(thetaMode)),col='black',pos=4,cex=1.5)
  text(0.4,10,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
  text(0.4,8,paste('mean = ',as.character(round(mean1,4))),col='blue',pos=4,cex=1.5)
  text(0.4,6,paste('median = ',as.character(round(median1,4))),col='magenta',pos=4,cex=1.5)
  text(0.4,4,paste('mode = ',as.character(round(mode1,4))),col='green',pos=4,cex=1.5)
  text(0.4,2,paste('sd = ',as.character(round(sd1,4))),col='blue',pos=4,cex=1.5)
  text(0.2,0.6,'95% interval',col='magenta',pos=4,cex=1.5)
} # end of a == 4
# } # end a-loop
# dev.off()

png('Berger09TriangPostAll210616.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(2,2))  # allow for 2 x 2 panels in one plot
for(a in 1:4) {
if(a == 1) { # Berger09TriangPost30M210615
  # png('Berger09TriangPost30M210615.png',width=16,height=16,units='cm',res=300)
  set.seed(1953) # set seed for random number generators
  M = 30
  x = rtriangle(M,0,1,thetaMode)
  dtheta = 0.001; thetaA = seq(dtheta,1-dtheta,dtheta); L = length(thetaA)
  Like = numeric(L)
  for(i in 1:L) {
    theta = thetaA[i]
    p = dtriangle(x,0,1,theta) # likelihoods for single data points
    Like[i] = prod(p)          # joint likelihood L(x | theta)
  }
  BetaPrior = dbeta(thetaA,0.5,0.5)
  Post = Like*BetaPrior
  # normalize:
  qnorm = sum(Post)*dtheta
  Post = Post/qnorm
  (mean1 = sum(thetaA*Post)*dtheta)
  (var1 = sum((thetaA-mean1)^2*Post)*dtheta)
  (sd1 = sqrt(var1))
  (mode1 = thetaA[which.max(Post)])
  Lcdf = length(thetaA)
  CDF = numeric(Lcdf)
  CDF[1] = Post[1]*dtheta
  for(i in 2:Lcdf) CDF[i] = CDF[i-1] + Post[i]*dtheta
  (median1 = thetaA[which.min((CDF-0.5)^2)])
  (i95L = thetaA[which.min((CDF-0.025)^2)])
  (i95U = thetaA[which.min((CDF-0.975)^2)])
  xp = c(i95L,i95U); yp = c(0,0)
  plot(thetaA,Post,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1,cex=0.4,cex.lab=1.5)
  abline(v=thetaMode,col='black')
  abline(v=mean1,col='blue',lty=2)
  abline(v=mode1,col='green',lty=3)
  abline(v=median1,col='magenta',lty=4)
  lines(xp,yp,col='magenta',lwd=2)
  title(xlab=TeX('$\\theta$'),cex.lab=1.5)
  title(ylab=TeX('Posterior $p(\\theta | x)$'),line=2.3,cex.lab=1.5)
  xt = 0.2
  text(xt,5.5,TeX('$\\beta (0.5,0.5)\\, prior$'),col='black',pos=4,cex=1.5)
  text(xt,4.2,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
} # end a == 1
if(a == 2) {
  # png('Berger09TriangPost100M210615.png',width=16,height=16,units='cm',res=300)
  set.seed(1953) # set seed for random number generators
  M = 100
  x = rtriangle(M,0,1,thetaMode)
  dtheta = 0.001; thetaA = seq(dtheta,1-dtheta,dtheta); L = length(thetaA)
  Like = numeric(L)
  for(i in 1:L) {
    theta = thetaA[i]
    p = dtriangle(x,0,1,theta) # likelihoods for single data points
    Like[i] = prod(p)          # joint likelihood L(x | theta)
  }
  BetaPrior = dbeta(thetaA,0.5,0.5)
  Post = Like*BetaPrior
  # normalize:
  qnorm = sum(Post)*dtheta
  Post = Post/qnorm
  (mean1 = sum(thetaA*Post)*dtheta)
  (var1 = sum((thetaA-mean1)^2*Post)*dtheta)
  (sd1 = sqrt(var1))
  (mode1 = thetaA[which.max(Post)])
  Lcdf = length(thetaA)
  CDF = numeric(Lcdf)
  CDF[1] = Post[1]*dtheta
  for(i in 2:Lcdf) CDF[i] = CDF[i-1] + Post[i]*dtheta
  (median1 = thetaA[which.min((CDF-0.5)^2)])
  (i95L = thetaA[which.min((CDF-0.025)^2)])
  (i95U = thetaA[which.min((CDF-0.975)^2)])
  xp = c(i95L,i95U); yp = c(0,0)
  plot(thetaA,Post,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1)
  abline(v=thetaMode,col='black')
  abline(v=mean1,col='blue',lty=2)
  abline(v=mode1,col='green',lty=3)
  abline(v=median1,col='magenta',lty=4)
  lines(xp,yp,col='magenta',lwd=2)
  title(xlab=TeX('$\\theta$'),cex.lab=1.5)
  title(ylab=TeX('Posterior $p(\\theta | x)$'),line=2.3,cex.lab=1.5)
  xt = 0.2
  text(xt,14,TeX('$\\beta (0.5,0.5)\\, prior$'),col='black',pos=4,cex=1.5)
  text(xt,10,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
}
if(a == 3) {
  # png('Berger09TriangPostFlat30M210615.png',width=16,height=16,units='cm',res=300)
  set.seed(1953) # set seed for random number generators
  M = 30
  thetaMode = 0.1 # true mode
  x = rtriangle(M,0,1,thetaMode)
  dtheta = 0.001; thetaA = seq(dtheta,1-dtheta,dtheta); L = length(thetaA)
  Like = numeric(L) # matrix(data=NA,nrow=L,ncol=M)
  for(i in 1:L) {
    theta = thetaA[i]
    p = dtriangle(x,0,1,theta) # likelihoods for single data points
    Like[i] = prod(p) # joint likelihood L(x | theta)
  }
  Post = Like # flat prior
  # normalize:
  qnorm = sum(Post)*dtheta
  Post = Post/qnorm
  (mean1 = sum(thetaA*Post)*dtheta)
  (var1 = sum((thetaA-mean1)^2*Post)*dtheta)
  (sd1 = sqrt(var1))
  (mode1 = thetaA[which.max(Post)])
  Lcdf = length(thetaA)
  CDF = numeric(Lcdf)
  CDF[1] = Post[1]*dtheta
  for(i in 2:Lcdf) CDF[i] = CDF[i-1] + Post[i]*dtheta
  (median1 = thetaA[which.min((CDF-0.5)^2)])
  (i95L = thetaA[which.min((CDF-0.025)^2)])
  (i95U = thetaA[which.min((CDF-0.975)^2)])
  xp = c(i95L,i95U); yp = c(0,0)
  plot(thetaA,Post,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1,ylim=c(0,6))
  abline(v=thetaMode,col='black')
  abline(v=mean1,col='blue',lty=2)
  abline(v=mode1,col='green',lty=3)
  abline(v=median1,col='magenta',lty=4)
  lines(xp,yp,col='magenta',lwd=2)
  title(xlab=TeX('$\\theta$'),cex.lab=1.5)
  title(ylab=TeX('Posterior $p(\\theta | x)$'),line=2.3,cex.lab=1.5)
  xt = 0.2
  text(xt,5.5,'flat prior',col='black',pos=4,cex=1.5)
  text(xt,4.2,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
} # end a == 3
if(a == 4) {
  # png('Berger09TriangPostFlat100M210615.png',width=16,height=16,units='cm',res=300)
  set.seed(1953) # set seed for random number generators
  M = 100
  thetaMode = 0.1 # true mode
  x = rtriangle(M,0,1,thetaMode)
  dtheta = 0.001; thetaA = seq(dtheta,1-dtheta,dtheta); L = length(thetaA)
  Like = numeric(L) # matrix(data=NA,nrow=L,ncol=M)
  for(i in 1:L) {
    theta = thetaA[i]
    p = dtriangle(x,0,1,theta) # likelihoods for single data points
    Like[i] = prod(p) # joint likelihood L(x | theta)
  }
  Post = Like # flat prior
  # normalize:
  qnorm = sum(Post)*dtheta
  Post = Post/qnorm
  (mean1 = sum(thetaA*Post)*dtheta)
  (var1 = sum((thetaA-mean1)^2*Post)*dtheta)
  (sd1 = sqrt(var1))
  (mode1 = thetaA[which.max(Post)])
  Lcdf = length(thetaA)
  CDF = numeric(Lcdf)
  CDF[1] = Post[1]*dtheta
  for(i in 2:Lcdf) CDF[i] = CDF[i-1] + Post[i]*dtheta
  (median1 = thetaA[which.min((CDF-0.5)^2)])
  (i95L = thetaA[which.min((CDF-0.025)^2)])
  (i95U = thetaA[which.min((CDF-0.975)^2)])
  xp = c(i95L,i95U); yp = c(0,0)
  plot(thetaA,Post,type='l',lwd=3,col='blue',xlab=NA,ylab=NA,las=1)
  abline(v=thetaMode,col='black')
  abline(v=mean1,col='blue',lty=2)
  abline(v=mode1,col='green',lty=3)
  abline(v=median1,col='magenta',lty=4)
  lines(xp,yp,col='magenta',lwd=2)
  xt = 0.2
  text(xt,14,'flat prior',col='black',pos=4,cex=1.5)
  text(xt,10,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
} # end of a == 4
} # end a-loop
# dev.off()