print('file: PDsPDFsStudent-t-NormalCDFs.R')
# CDFs of t distributions & standard normal PDF
t = seq(-3,3,0.01); 
t3 = pt(t,3); t5 = pt(t,5); t10 = pt(t,10);
N_CDF =  pnorm(t * sqrt(2))
# png('tDistCDFNormalBook160103R.png',width=16,height=16,units='cm',res=300)
plot(t,t3,type='l',col='blue',xlab='t',ylab='CDF',
     xlim=c(-3,3), ylim=c(0,1),lwd=3,las=1,lty=4,cex.lab=1.5)
lines(t,t5,col='red',lwd=2,lty=2)
lines(t,t10,col='black',lwd=2,lty=3)
lines(t,N_CDF,col='magenta',lwd=3,lty=1)
xt = -2;
text(xt,0.3,TeX('$\\nu = 10$'),col='black',cex=1.5)
text(xt,0.5,TeX('$\\nu = 5$'),col='red',cex=1.5)
text(xt,0.7,TeX('$\\nu = 3$'),col='blue',cex=1.5)
text(-3,0.95,'standard normal distribution',col='magenta',cex=1.5,pos=4)
# dev.off()