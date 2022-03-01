print('file: PDsPDFsExpPDF.R')
# exponential PDF
# f(x; lambda) = lambda * exp(-lambda*x) for x >= 0
lambda = 1
x = seq(0,5,0.001)
y = dexp(x,rate=lambda)
library(latex2exp)
# png('dexp160216.png',width=16,height=12,units='cm',res=300)
plot(x,y,type='l',lwd=3,col='blue',xlab='x',las=1,ylab=NA,cex.lab=1.5)
title(ylab=TeX('$Exponential\\, PDF\\, f(x; \\lambda = 1)$'),cex.lab=1.5,line=2.3)
# dev.off()