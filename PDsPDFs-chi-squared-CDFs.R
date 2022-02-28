print('file: PDsPDFs-chi-squared-CDFs.R')
# CDFs of chi-squared distributions
x = seq(0.0,20,0.01)
chisq3 = pchisq(x,3); chisq5 = pchisq(x,5); chisq10 = pchisq(x,10)
# png('chi-squaredCDFBook160143R.png',width=16,height=12,units='cm',res=300)
plot(x,chisq3,type='l',col='blue',xlab='x',ylab='CDFs for chi-squared PDFs',
     lwd=3,las=1,cex.lab=1.5)
lines(x,chisq5,col='red',lwd=3,lty=2)
lines(x,chisq10,col='black',lwd=3,lty=3)
text(3,0.9,expression(paste(nu,' = 3')),col='blue',cex=1.5)
text(6.3,0.5,expression(paste(nu,' = 5')),col='red',cex=1.5)
text(10,0.3,expression(paste(nu,' = 10')),col='black',cex=1.5)
# dev.off()