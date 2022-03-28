print('file: PDsPDFsPoisson2Examples.R')
lambda1 = 0.55
lambda2 = 3.72
k = seq(0,10)
p1 = dpois(k,lambda1)
p2 = dpois(k,lambda2)
# png('PoissonPDsEx220328.png',width=16,height=16,units='cm',res=300)
plot(k,p1,type='p',lwd=4,col='black',xlab='k',ylab='Poisson probability distributions',
     las=1,cex=0.6,cex.lab=1.5)
points(k,p2,lwd=4,col='magenta',cex=0.6,pch=24)
text(3,0.5,bquote(~lambda[1] == .(lambda1)),col='black',pos=4,cex=1.5)
text(3,0.4,bquote(~lambda[2] == .(lambda2)),col='magenta',pos=4,cex=1.5)
# dev.off()