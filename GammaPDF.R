print('file: GammaPDF.R')
x = seq(0.01,10,0.01)
# Shape-rate definition: Robert (2007): alpha, beta
alpha = 1.5; beta = 0.5;
y = dgamma(x,shape=alpha,rate=beta)
# png('GammaPDFscale220401.png',width=16,height=12,units='cm',res=300)
plot(x,y,type='l',lwd=3,col='blue',xlab='x',ylab='Density',las=1,cex.lab=1.5)
text(6,0.23,bquote(~alpha == .(alpha)),col='blue',pos=4,cex=1.5)
text(6,0.19,bquote(~beta == .(beta)),col='blue',pos=4,cex=1.5)
# dev.off()