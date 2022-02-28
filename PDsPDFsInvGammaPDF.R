print('file: PDsPDFsInvGammaPDF.R')
# inverse-gamma PDF
# install.packages('LaplacesDemon')
library(LaplacesDemon) 
dx = 0.01; x = seq(dx,2,dx)
phi = 2.3; psi = 0.5
y1 = dinvgamma(x,phi,psi)
library(latex2exp)
# png('InverseGamma210618.png',width=16,height=12,units='cm',res=300)
plot(x,y1,type='l',lwd=3,col='blue',xlab='x',ylab=NA,las=1,cex=0.4,cex.lab=1.5)
xt = 1
text(xt,2.5,TeX('$\\phi = 2.3$'),col='blue',pos=4,cex=1.5)
text(xt,2,TeX('$\\psi = 0.5$'),col='blue',pos=4,cex=1.5)
title(ylab=TeX('Inverse-gamma PDF $p(x | \\phi,\\psi )$'),cex.lab=1.5,line=2.3)
# dev.off()