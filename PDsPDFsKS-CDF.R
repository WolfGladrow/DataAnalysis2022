print('file: PDsPDFsKS-CDF.R')
# Kolmogorov-Smirnov-CDF
# install.packages('kolmim')
library(kolmim)
xpD = seq(0.01,0.99,0.01); LpD = length(xpD)
ypD15 = numeric(LpD); ypD30 = numeric(LpD); ypD100 = numeric(LpD)
for(k in 1:LpD) {ypD15[k] = pkolm(xpD[k],15);
ypD30[k] = pkolm(xpD[k],30);
ypD100[k] = pkolm(xpD[k],100)}
# png('KSCDF161223.png',width=16,height=16,units='cm',res=300)
plot(xpD,ypD15,type='l',col='blue',lwd=3,xlab='D',ylab='CDF',las=1,cex.lab=1.5)
lines(xpD,ypD30,col='black',lwd=3,lty=2)
lines(xpD,ypD100,col='red',lwd=3,lty=3)
text(0.5,0.8,'n = 100',col='red',cex=1.5)
text(0.5,0.7,'n = 30',col='black',cex=1.5)
text(0.5,0.6,'n = 15',col='blue',cex=1.5)
# dev.off()
