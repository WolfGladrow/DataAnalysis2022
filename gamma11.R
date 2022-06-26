print('file: gamma11.R')
library(latex2exp)
dx = 0.01; x = seq(dx,5,dx) 
alpha = 1; beta = 1
y = dgamma(x,alpha,beta)
# png('Gamma11a220626.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='l',lwd=4,col='blue',xlab=TeX('$\\lambda$'),ylab=NA,las=0,
       cex=0.6,cex.lab=1.5,ylim=c(0,max(y)))
title(ylab=TeX('$Gamma(\\lambda;1,1)$'),line=2.3,cex.lab=1.5)
# dev.off()
s = 1792+alpha; n = 2306+beta
Lest = (s+1)/n;      print(c(round(Lest,5),'Lest'))
uLest = sqrt(s+1)/n; print(c(round(uLest,5),'uLest'))
# flat:
alphaF = 1; betaF = 0
s = 1792+alphaF; n = 2306+betaF
LestF = (s+1)/n;      print(c(round(LestF,5),'LestF'))
uLestF = sqrt(s+1)/n; print(c(round(uLestF,5),'uLestF'))