print('file: PDsPDFsNormalStandard.R')
# standard normal distribution 
x = seq(-3,3,0.01); y = dnorm(x)
library(latex2exp)
# png('NormalDistStdBook220225.png',width=16,height=12,units='cm',res=300)
plot(x,y,type='l',xlab='x',ylab='Standard normal distribution',col='blue',lwd=3,las=1) 
text(0,0.15,TeX('$\\mu = 0$'),col='blue',cex=2)
text(0,0.1,TeX('$\\sigma = 1$'),col='blue',cex=2)
# dev.off()