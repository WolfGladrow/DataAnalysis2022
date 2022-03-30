print('file: PDsPDFsTransformPDFs.R')
# t-PDF for mu-ymean/sqrt(s^2/n); coordinate transformation
n = 5; ymean = 1.7; ssq = 2; 
dmu = ymean/100; mu = seq(0,2*ymean,dmu)
tmu = (mu-ymean)/sqrt(ssq/n)
y = dt(tmu,n-1)/sqrt(ssq/n)
x = seq(-3,4,0.01); yt = dt(x,n-1)
print(c(round(1/sqrt(ssq/n),2),'1/sqrt(ssq/n)'))
print(c(round(max(yt),4),'max t-PDF'))
print(c(round(max(y),4),'max scaled t-PDF'))
library(latex2exp)
# png('TransformVarPDF210518.png',width=16,height=16,units='cm',res=300)
plot(mu,y,type='l',lwd=4,col='blue',
     xlab=TeX('$\\mu$'),ylab=NA,las=1,cex=0.4,cex.lab=1.5)
title(ylab=TeX('$p(\\mu | n, \\bar{y}, s^2); \\, p(t; n-1)$'),line=2.3,cex.lab=1.5)
lines(x,yt,col='black',lty=2)
legend('topleft',legend=c('transformed t','standard t'),col=c('blue','black'),
       lty=c(1,2),lwd=c(4,1),cex=1.2)
# dev.off()