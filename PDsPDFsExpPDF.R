print('file: PDsPDFsExpPDF.R')
# exponential PDF
# f(x; lambda) = lambda * exp(-lambda*x) for x >= 0
lambda = 1; x = seq(0,6,0.001); y = dexp(x,rate=lambda)
lambda2 = 0.6; y2 = dexp(x,rate=lambda2)
# png('dexp160216.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='l',lwd=4,col='black',xlab='x',las=1,ylab='Density',cex.lab=1.5,xlim=c(0,5))
lines(x,y2,col='magenta',lwd=2,lty=4)
text(2,0.95,bquote(~lambda == .(lambda)),col='black',cex=1.5,pos=4)
text(2,lambda2,bquote(~lambda == .(lambda2)),col='magenta',cex=1.5,pos=4)
# dev.off()