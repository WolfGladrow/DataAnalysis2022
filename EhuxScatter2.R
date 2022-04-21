print('file: EhuxScatter2.R')
# Scatter plot of 2 data sets; 0,ymax: Ehux growth rates
# specific growth rates (d^-1) of Emiliana huxleyi 
Y1=c(0.63,0.78,0.57,0.66,0.71)
Y2=c(0.57,0.58,0.65,0.46)
ymin = min(Y1,Y2); ymax = max(Y1,Y2)
Y1mean = mean(Y1); Y2mean = mean(Y2)
library(latex2exp)
# png('Ehux1PointMissing1710c.png',width=16,height=16,units='cm',res=300)
plot(Y1,type='p',lwd=4,col='blue',xlab='Data #',ylab=NA,las=1,cex=0.6,
     ylim=c(0,ymax),cex.lab=1.5)
title(ylab=TeX('$Specific\\, growth\\, rate\\, (d^{-1})$'),line=2.3,cex.lab=1.5)
points(Y2,col='black',lwd=4,cex=0.6,pch=24)
abline(Y1mean,0,col='blue',lty=2)
abline(Y2mean,0,col='black',lty=4)
# dev.off()