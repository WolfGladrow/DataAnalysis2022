print('file: PDsPDFsStudent-t-Normal.R')
# t distributions & standard normal PDF
t = seq(-3,3,0.01); N = dnorm(t)
t3 = dt(t,3); t5 = dt(t,5); t10 = dt(t,10)
library(latex2exp)
# png('tDistNormalBook160103R.png',width=16,height=16,units='cm',res=300)
plot(t,t3,type='l',col='blue',xlab='t',ylab='Density',
     xlim=c(-3,3), ylim=c(0,0.45),lwd=3,las=1,lty=4,cex.lab=1.5)
lines(t,t5,col='red',lwd=2,lty=2)
lines(t,t10,col='black',lwd=2,lty=3)
lines(t,N,col='magenta',lwd=3,lty=1)
xt = 0;
text(xt,0.2,TeX('$\\nu = 10$'),col='black',cex=1.5)
text(xt,0.15,TeX('$\\nu = 5$'),col='red',cex=1.5)
text(xt,0.1,TeX('$\\nu = 3$'),col='blue',cex=1.5)
text(0,0.425,'standard normal distribution',col='magenta',cex=1.5)
# dev.off()