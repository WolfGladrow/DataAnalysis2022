print('file: PDsPDFsLogNormalPDF.R')
# standard log-normal distribution
x = seq(0,3,0.01); LN = dlnorm(x)
# png('LogNormalPDFStdBook160405.png',width=16,height=16,units='cm',res=300)
plot(x,LN,type='l',xlab='x',ylab='Density',col='blue',lwd=4,las=1,cex.lab=1.5) 
# dev.off()