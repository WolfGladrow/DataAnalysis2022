print('file: PDsPDFsBetaPDF.R')
# beta PDF
alpha = 2; beta = 1.5
parr = seq(0.0,1,0.001)
betaPDF = dbeta(x=parr,shape1=alpha,shape2=beta)
# png('betaPDF160818.png',width=16,height=12,units='cm',res=300)
plot(parr,betaPDF,type='l',lwd=3,col='blue',xlab='p',
     ylab='Beta PDF',las=1,cex.lab=1.5)
text(0.5,0.8,expression(paste(alpha,' = 2')),col='blue',cex=1.5)
text(0.5,0.6,expression(paste(beta,' = 1.5')),col='blue',cex=1.5)
# dev.off()