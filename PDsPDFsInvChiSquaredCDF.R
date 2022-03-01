print('file: PDsPDFsInvChiSquaredCDF.R')
# CDF of inverse chi-squared PDF
invChiSqCDF = function(x,nu) {pgamma(nu/2,1/(2*x))}
x = seq(0.01,1,0.001)
chisq3 = invChiSqCDF(x,3); chisq5 = invChiSqCDF(x,5); chisq10 = invChiSqCDF(x,10)
# png('invchi-squaredCDFBook160104R.png',width=16,height=12,units='cm',res=300)
plot(x,chisq3,type='l',col='blue',xlab='t',ylab='Chi-squared distributions',lwd=3,
     ylim=c(0,1),las=1,cex.lab=1.5)
lines(x,chisq5,col='red',lwd=3,lty=2); lines(x,chisq10,col='black',lwd=3,lty=3)
xt = 0.5
text(xt,0.5,expression(paste(nu,' = 10')),col='black',cex=1.5)
text(xt,0.4,expression(paste(nu,' = 5')),col='red',cex=1.5)
text(xt,0.3,expression(paste(nu,' = 3')),col='blue',cex=1.5)
# dev.off()