print('file: PDsPDFsPoissonPDexample.R')
# Poisson distribution (example)
lambda1 = 2.7; k = seq(0,10); pk = dpois(k,lambda1) 
# png('PoissonPD160817.png',width=16,height=16,units='cm',res=300)
plot(k,pk,type='p',lwd=4,col='blue',xlab='k',ylab='Probability',las=1,
     cex=0.6,cex.lab=1.5)
text(7,0.2,bquote(~lambda == .(lambda1)),col='blue',cex=1.5)
# dev.off()