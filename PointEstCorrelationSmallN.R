print('file: PointEstCorrelationSmallN.R')
# correlation estimator: small n
set.seed(1953)
source('sampleUcorFct.R')
r = 0.7 # desired (specified) correlation
n = 10  # sample size
M = 1e3 # number of Monte Carlo runs
corEst = numeric(M)
for(k in 1:M) {q=sampleUcorFct(r,n); x=q[1:n]; y=q[(n+1):(2*n)]; corEst[k] = cor(x,y)}
sflag = 2
if (sflag == 1) {
  # png('CovEstMC200708.png',width=16,height=16,units='cm',res=300)
  hist(covEst,breaks=33,col='blue',main='',las=1,cex.lab=1.5,xlab='Covariance')
  abline(v=0.0417,col='black',lty=2) # covariance 0.0417 estimate from MC simulation with large n
  # dev.off()
}
if (sflag == 2) {
  # png('CorEstMC200708.png',width=16,height=16,units='cm',res=300)
  hist(corEst,breaks=33,col='blue',main='',las=1,cex.lab=1.5,xlab='Correlation')
  abline(v=r,col='black',lty=2)
  # dev.off()
}