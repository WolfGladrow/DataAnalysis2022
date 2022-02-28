print('file: PDsPDFs-F-PDFs.R')
# F distributions
x = seq(0.01,2,0.01)
F11 = df(x,1,1); F15 = df(x,1,5); F51 = df(x,5,1); F8075 = df(x,80,75)
# png('FpdfBook160103R.png',width=16,height=12,units='cm',res=300)
plot(x,F15,type='l',xlab='x',ylab=NA,col='black',lwd=3,las=1,cex.lab=1.5) 
title(ylab=TeX('$F(x; \\nu_1, \\nu_2)$'),cex.lab=1.5,line=2.3)
lines(x,F11,col='blue',lwd=3,lty=2)
lines(x,F51,col='red',lwd=3,lty=3)
lines(x,F8075,col='magenta',lwd=3,lty=4)
xt = 0.5
text(xt,3.5,TeX('$\\nu_1 = 1,\\, \\nu_2 = 5$'),col='black',cex=1.5)
text(xt,2.5,TeX('$\\nu_1 = 1,\\, \\nu_2 = 1$'),col='blue',cex=1.5)
text(xt,3,TeX('$\\nu_1 = 5,\\, \\nu_2 = 1$'),col='red',cex=1.5)
text(xt,2.0,TeX('$\\nu_1 = 80,\\, \\nu_2 = 75$'),col='magenta',cex=1.5)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# cex.lab=1.5   increase size of axis labels (default: cex.lab=1)
# ----------------------------------------------------------------