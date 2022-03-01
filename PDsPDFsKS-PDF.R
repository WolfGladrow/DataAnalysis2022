print('file: PDsPDFsKS-PDF.R')
# Kolmogorov-Smirnov-PDF; calculation takes less than 1 min
# install.packages('kolmim')
library(kolmim)
xpD = seq(0.01,0.99,0.01); LpD = length(xpD)
ypD15 = numeric(LpD); ypD30 = numeric(LpD); ypD100 = numeric(LpD)
for(k in 1:LpD) {ypD15[k] = pkolm(xpD[k],15);
ypD30[k] = pkolm(xpD[k],30);
ypD100[k] = pkolm(xpD[k],100)}
# KS-PDF: differentiate KS-CDF
dx = 0.001; dx2i = 1/(2*dx)
xPDF = seq(dx,0.999,dx); LPDF = length(xPDF)
yPDF15 = numeric(LPDF)
yPDF30 = numeric(LPDF)
yPDF100 = numeric(LPDF)
for (k in 2:(LPDF-1)) {
    yPDF15[k] = (pkolm(xPDF[k+1],15)-pkolm(xPDF[k-1],15))*dx2i;
    yPDF30[k] = (pkolm(xPDF[k+1],30)-pkolm(xPDF[k-1],30))*dx2i;
    yPDF100[k] = (pkolm(xPDF[k+1],100)-pkolm(xPDF[k-1],100))*dx2i}
# png('KSPDF161223.png',width=16,height=12,units='cm',res=300)
plot(xPDF,yPDF15,type='l',col='blue',lwd=3,xlab='D',
       ylab='Kolmogorov-Smirnov PDF',las=1,ylim=c(0,17),cex.lab=1.5)
lines(xPDF,yPDF30,col='black',lwd=3,lty=2)
lines(xPDF,yPDF100,col='red',lwd=3,lty=3)
text(0.5,15,'n = 100',col='red',cex=1.5)
text(0.5,10,'n = 30',col='black',cex=1.5)
text(0.5,5,'n = 15',col='blue',cex=1.5)
# dev.off()