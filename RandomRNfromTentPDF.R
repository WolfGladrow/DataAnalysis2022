print('file: RandomRNfromTentPDF.R')
# tent PDF -> F(y) -> F^{-1}(x)
dy = 0.01; y1 = seq(0,0.5,dy); y2 = seq(0.5,1,dy);
q1 = 4*y1; q2 = 4*(1-y2);
F1 = 2*y1^2;  F2 = 4*y2-1-2*y2^2;
dx = 1/8/50; x1 = seq(0,0.5,dx); y1x = sqrt(x1/2);
x2 = seq(1/2,1,dx); y2xp = 1+sqrt(1-x2/2-1/2); 
y2xm = 1-sqrt(1-x2/2-1/2);
library(latex2exp)
# png('tentA160722.png',width=16,height=12,units='cm',res=300)
par(mfrow=c(2,2))  # 2 x 2 panels in one plot
# subplot 1:
# create an empty plot with right kind of size:
plot(1,type='n',xlim=c(-0.1,1.1),ylim=c(0,2.1),xlab='y',ylab='q(y)',las=1,cex.lab=1.5)
lines(y1,q1,type='l',lwd=3,col='blue',xlab='y',ylab='q(y)')
lines(y2,q2,type='l',lwd=3,col='red')
# subplot 2:
plot(1,type='n',xlim=c(-0.1,1.1),ylim=c(0,1.1),xlab='y',ylab='F(y)',las=1,cex.lab=1.5)
lines(y1,F1,type='l',lwd=3,col='blue',xlab='y',ylab='F(y)')
lines(y2,F2,type='l',lwd=3,col='red')
# subplot 3:
plot(1,type='n',xlim=c(-0.1,1.1),ylim=c(-0.5,1.5),xlab='x',ylab=NA,las=1,cex.lab=1.5)
title(ylab=TeX('$F^{-1}(x)$'),line=2.3,cex.lab=1.5)
lines(x1,y1x,type='l',lwd=3,col='blue')
lines(x2,y2xm,type='l',lwd=3,col='red')
lines(x2,y2xp,type='l',col='black',pch=22,lty=2)
lines(x1,-y1x,type='l',col='black',pch=22,lty=2)
# dev.off()