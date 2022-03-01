print('file: PDsPDFsGammaPDF.R')
# gamma PDF
# dgamma(x, shape, rate = 1, scale = 1/rate, log = FALSE)
alpha = 2
xarr = seq(0.01,10,0.01)
gPDF = dgamma(x=xarr, shape=alpha)
# png('gPDF160818.png',width=16,height=12,units='cm',res=300)
plot(xarr,gPDF,type='l',lwd=3,col='blue',xlab='x',ylab='Gamma PDF',las=1,cex.lab=1.5)
text(7,0.3,expression(paste(alpha,' = 2')),col='blue',cex=1.5)
# dev.off()