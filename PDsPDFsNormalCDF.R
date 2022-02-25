print('file: PDsPDFsNormalCDF.R')
# CDF of normal distribution
x = seq(-4,4,0.01); NCDF =  pnorm(x)
# png('NormalCDFstandardBook160103R.png',width=16,height=12,units='cm',res=300)
plot(x,NCDF,type='l',col='blue',lwd=3,xlab='x',
     ylab='CDF of standard normal distribution',las=1,xlim=c(-3,3))
# dev.off()