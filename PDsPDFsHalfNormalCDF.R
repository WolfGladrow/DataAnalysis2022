print('file: PDsPDFsHalfNormalCDF.R')
# CDF of half-normal distribution
x0 = seq(-4,4,0.01); NCDF0 =  pnorm(x0)
erf = function(x) 2 * pnorm(x * sqrt(2)) - 1
sigma = 1
x = seq(0,4,0.01); HNCDF = erf(x/(sigma*sqrt(2)))
# png('HalfNormalCDF200705.png',width=16,height=12,units='cm',res=300)
plot(x0,NCDF0,type='l',col='blue',lwd=1,xlab='x',las=1,lty=2,
     ylab='CDF of half-normal distribution',xlim=c(-3,3),ylim=c(0,1),cex.lab=1.5)
lines(x,HNCDF,col='black',lwd=3)
xp = c(-4,0); yp = c(0,0)
lines(xp,yp,col='black',lwd=3)
legend('topleft',legend=c('half normal','normal'),col=c('black','blue'),
       lty=c(1,2),lwd=c(3,1))
# dev.off()