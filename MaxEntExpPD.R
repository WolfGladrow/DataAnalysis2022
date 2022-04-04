print('file: MaxEntExpPD.R')
# MaxEnt: discrete exponential PD
mu = 2.5
j = seq(0,50)
pj = 1/(1+mu)*(mu/(1+mu))^j
# png('ExpDisMu2p5X171212.png',width=16,height=16,units='cm',res=300)
plot(j,pj,type='p',lwd=4,col='blue',xlab='j',
     ylab=NA,las=0,cex=0.6,xlim=c(0,20),cex.lab=1.5)
title(ylab=expression(p[j]),line=2.3,cex.lab=1.5)
text(8,0.2,bquote(~mu == .(mu)),col='blue',pos=4,cex=1.5)
# dev.off()