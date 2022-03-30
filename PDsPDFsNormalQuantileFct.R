print('file: PDsPDFsNormalQuantileFct.R')
# quantile function for standard normal PDF
p = seq(0.001,0.999,0.001)
x = qnorm(p)
# png('QuantileStdNormal190620.png',width=16,height=16,units='cm',res=300)
plot(p,x,type='l',lwd=3,col='blue',xlab='p',ylab='x',las=1,cex.lab=1.5)
# dev.off()