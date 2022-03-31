print('file: PDsPDFsLogNormalCDF.R')
# CDF of standard log-normal distribution
x = seq(0,5,0.01); CDFLN = plnorm(x)
# png('LogNormalCDFStdBook160405.png',width=16,height=16,units='cm',res=300)
plot(x,CDFLN,type='l',xlab='x',ylab='CDF',ylim=c(0,1),col='blue',lwd=4,las=1,cex.lab=1.5) 
# dev.off()