print('file: PDsPDFsGeometricPDexample.R')
# geometric PD
h = 0.3        # probability of success in single trial
k = seq(1,10)  # number of trials until first success
pgeo = dgeom(k-1,h)
# dgeom(x, prob, log = FALSE)
# x  vector of quantiles representing the number of failures in a sequence 
#    of Bernoulli trials before success occurs.
# Remark: number of failures before success = k-1 where 
#         k = number of trials until first success
# png('GeometricPD170214.png',width=16,height=12,units='cm',res=300)
plot(k,pgeo,type='p',lwd=3,col='blue',xlab='Number of trials k until first success',
     ylab='Probabilities of geometric PD',las=1,cex=0.4,ylim=c(0,max(pgeo)))
abline(h=0,col='green',lty=4)
# dev.off()