print('file: PDsPDFsNormalDifferentMeans.R')
# normal distributions with different means
library(latex2exp)
# png('NormalDistDiffMeanBook220225.png',width=16,height=16,units='cm',res=300)
x = seq(-4,4,0.01); N = dnorm(x); Nm1 = dnorm(x,-1); Np1 = dnorm(x,1)
plot(x,N,type='l',xlab='x',ylab='Normal distributions',col='blue',lwd=3,
     xlim = c(-4,4),ylim=c(0,0.48),las=1,cex.lab=1.5) 
lines(x,Nm1,col='black',lwd=3,lty=2)
lines(x,Np1,col='red',lwd=3,lty=3)
yt = 0.44
text(0,0.46,TeX('$\\mu = 0$'),col='blue',cex=1.5)
text(0,0.42,TeX('$\\sigma = 1$'),col='blue',cex=1.5)
text(-1.2,0.46,TeX('$\\mu = -1$'),col='black',cex=1.5)
text(-1.2,0.42,TeX('$\\sigma = 1$'),col='black',cex=1.5)
text(1.2,0.46,TeX('$\\mu = 1$'),col='red',cex=1.5)
text(1.2,0.42,TeX('$\\sigma = 1$'),col='red',cex=1.5)
# dev.off()