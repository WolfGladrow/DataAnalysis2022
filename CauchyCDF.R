print('file: CauchyCDF.R')
# CDF of Cauchy distribution
x = seq(-5.0,5,0.01)
Cauchy = pcauchy(x)
# png('CauchyCDFBook160143R.png',width=16,height=16,units='cm',res=300)
plot(x,Cauchy,type='l',col='blue',xlab='x',ylab='CDF',lwd=4,
     xlim=c(min(x),max(x)),ylim=c(0,1),las=1,cex.lab=1.5)
# dev.off()