print('file: PDsPDFsInvChiSquaredPDF.R')
# inverse chi-squared PDF
# source('invChiSq.R')
invChiSq = function(x,nu) {
  icsq = 2^(-nu/2)/gamma(nu/2)*x^(-nu/2-1)*exp(-1/(2*x)); 
  return(icsq)}
x = seq(0.01,1,0.001)
chisq3 = invChiSq(x,3); chisq5 = invChiSq(x,5); chisq10 = invChiSq(x,10)
# png('invchi-squaredBook160104R.png',width=16,height=16,units='cm',res=300)
plot(x,chisq3,type='l',col='blue',xlab='t',ylab='Density',lwd=4,
     ylim=c(0,10),las=1,cex.lab=1.5)
lines(x,chisq5,col='red',lwd=3,lty=2); lines(x,chisq10,col='black',lwd=3,lty=3)
xt = 0.5
text(xt,9,expression(paste(nu,' = 10')),col='black',cex=1.5)
text(xt,4,expression(paste(nu,' = 5')),col='red',cex=1.5); 
text(xt,2,expression(paste(nu,' = 3')),col='blue',cex=1.5)
# dev.off()