print('file: PDsPDFsGeometricCDFexample.R')
# geometric CDF
h = 0.3        # probability of success in a single trial
k = seq(1,10,0.01)  # number of trials until first success
CDFgeo = pgeom(k-1,h)
# dgeom(x, prob, log = FALSE)
# x  vector of quantiles representing the number of failures in a sequence 
#    of Bernoulli trials before success occurs.
# Remark: number of failures before success = k-1 where 
#         k = number of trials until first success
# png('GeometricCDF170214.png',width=16,height=12,units='cm',res=300)
plot(k,CDFgeo,type='l',lwd=3,col='blue',xlab='Number of trials, k, until first success',
     ylab='Geometric CDF',las=1,cex=0.4,ylim=c(0,1))
abline(h=1,col='green',lty=4)
# dev.off()