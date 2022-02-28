print('file: PDsPDFs-chi-squared-PDFs.R')
# chi-squared PDF
x = seq(0.0,10,0.01)
chisq3 = dchisq(x,3); chisq5 = dchisq(x,5); chisq10 = dchisq(x,10)
# png('chi-squaredBook160143R.png',width=16,height=12,units='cm',res=300)
plot(x,chisq3,type='l',col='blue',xlab='x',ylab='Chi-squared distributions',
     lwd=3,las=1,cex.lab=1.5)
lines(x,chisq5,col='red',lwd=3,lty=2)
lines(x,chisq10,col='black',lwd=3,lty=3)
text(3,0.23,expression(paste(nu,' = 3')),col='blue',cex=1.5)
text(4,0.17,expression(paste(nu,' = 5')),col='red',cex=1.5)
text(8,0.12,expression(paste(nu,' = 10')),col='black',cex=1.5)
# dev.off()