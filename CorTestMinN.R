print('file: CorTestMinN.R')
print(date())
# minimum n for sig. correlation test
alpha = 0.05; r = 0.6; n1 = 6; n2 = 50   # n range tested (depends on considered r range)
ra = seq(0.3,0.9,0.001); L = length(ra)
nmin = numeric(L)
for(k in 1:L) {r = ra[k]; firstflag = 0;
  for(n in n1:n2) {
    tcor=r/sqrt((1-r^2)/(n-2)); 
    p = 2*pt(-abs(tcor),df=n-2);
    if (p < alpha) {if (firstflag == 0) {nmin[k] = n; firstflag = 1}}}}
library(latex2exp)
# png('CorrelationTestnmin170710.png',width=16,height=16,units='cm',res=300)
plot(ra,nmin,type='l',lwd=4,col='blue',xlab='Magnitude of observed correlation, |r|',
     ylab=NA,las=1,cex=0.4,ylim=c(0,45),cex.lab=1.5)
title(ylab=TeX('$n_{min} \\, (|r|)$'),cex.lab=1.5,line=2.3)
# dev.off()