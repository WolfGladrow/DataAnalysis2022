print('file: CorTestStatistic.R')
# Correlation different from zero? Test statistic t_cor (r) for n=6 
r = seq(-0.99,0.99,0.01); n = 6
tcor=r/sqrt((1-r^2)/(n-2))
# png('tcorOVERr170516.png',width=16,height=12,units='cm',res=300)
plot(r,tcor,type='l',lwd=3,col='blue',las=1,xlab='Correlation coefficient r',
     ylab='',cex.lab=1.5)
title(ylab=expression(paste(t[cor],' (r)')),line=2,cex.lab=1.5)
# dev.off()