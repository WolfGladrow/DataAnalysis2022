print('file: PointEstCorrelation.R')
# covariance & correlation estimators (7/2020)')
print(date())
set.seed(1953)
source('sampleUcorFct.R')
r = 0.7; print(c(r,'desired (specified) correlation')) 
narr = seq(5,500); L = length(narr)
covEst1 = numeric(L); covEst2 = numeric(L)
corEst1 = numeric(L); corEst2 = numeric(L)
for(k in 1:L) {
  n = narr[k];
  q=sampleUcorFct(r,n); x=q[1:n]; y=q[(n+1):(2*n)]; covEst1[k] = cov(x,y)
  corEst1[k] = cor(x,y)
  q=sampleUcorFct(r,n); x=q[1:n]; y=q[(n+1):(2*n)]; covEst2[k] = cov(x,y)
  corEst2[k] = cor(x,y)
}
nLarge = 1e5
out = sampleUcorFct(r,nLarge)
xL = out[1:nLarge]; yL = out[(nLarge+1):(2*nLarge)]
print(c(round(cor(xL,yL),4),'correlation'))
print(c(round(cov(xL,yL),4),'covariance'))
# png('CorrelationEst200707.png',width=16,height=16,units='cm',res=300)
plot(narr,corEst1,log='x',type='p',lwd=4,col='blue',xlab='log(n)',
     ylab='Correlation',las=1,cex=0.6,cex.lab=1.5)
points(narr,corEst2,col='red',pch=24,lwd=4,cex=0.6)
abline(h=r,col='black',lty=2)
# dev.off()
# -----------------------------------------------------------------------------
# Results:
# "file: PointEstCorrelation.R"
# "Sun Dec 18 11:01:15 2022"
# "0.7"         "desired (specified) correlation"
# "0.7023"      "correlation"
# "0.0417"      "covariance"
# -----------------------------------------------------------------------------
# Remarks:
# Test of the routine sampleUcorFct(r,n) that generates 2 sample (x and y) of the
#   same size (n) and that are approximately correlated with a specified 
#   correlation coefficient (r).
# For small sample sizes the correlation between the samples can deviate quite a
#   bit from the specified correlation (r), however, deviations tend to become
#   smaller with increasing sample size.
# -----------------------------------------------------------------------------
