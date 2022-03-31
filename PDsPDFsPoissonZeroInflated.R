print('file: PDsPDFsPoissonZeroInflated.R')
# Zero Inflated Poisson distribution
lambda = 2.7; k = seq(0,10); 
p = 0.4 # zero-inflation parameter
# install.packages('VGAM')
library(VGAM)
pkZIP = dzipois(k,lambda,p)
# png('PoissonZIP191105.png',width=16,height=16,units='cm',res=300)
plot(k,pkZIP,type='p',lwd=4,col='black',xlab='k',
     ylab='Probability',las=1,cex=0.6,cex.lab=1.5)
text(2.5,0.25,bquote(~lambda == .(lambda)),col='black',pos=4,cex=1.5)
text(2.5,0.35,bquote(~p == .(p)),col='black',pos=4,cex=1.5)
# dev.off()
print(c(round(pkZIP[1],4),'pkZIP[1]'))