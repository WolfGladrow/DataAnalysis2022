print('file: BootstrapMeanNormal.R')
# estimate uncertainty of mean from standard normal PDF via bootstrapping
n = 30 # sample size
set.seed(1953)
r = rnorm(n) # sample from standard normal PDF
print(c(round(mean(r),4),'mean(r)'))
print(c(round(1/sqrt(n),4),'1/sqrt(n)'))
B = 1000 # number of bootstrap samples
xrange = seq(1,n)
bm = numeric(B)
for(j in 1:B) {
  i = sample(x=xrange,n,replace=TRUE) 
  bm[j] = mean(r[i])
}
# Uncertainty via standard deviation:
urBEst = sd(bm); print(c(round(urBEst,4),'Bootstrap estimate of uncertainty'))
urTheo = 1/sqrt(n); print(c(round(urTheo,4),'uncertainty = sigma/sqrt(n)'))
# 67% and 95% confidence intervals:
q = as.numeric(quantile(bm,c(0.33/2,1-0.33/2)))
print(c(round(q,4),'67% confidence interval (bootstrap)'))
q67 = qnorm(1-0.33/2,mean=0,sd=urTheo)
print(c(round(-q67,4),round(q67,4),'66% confidence interval (theory)'))
q = as.numeric(quantile(bm,c(0.05/2,1-0.05/2)))
print(c(round(q,4),'95% confidence interval (bootstrap)'))
q95 = qnorm(1-0.05/2,mean=0,sd=urTheo)
print(c(round(-q95,4),round(q95,4),'95% confidence interval (theory)'))
sflag = 1
if (sflag == 1) {
  # png('BootstrapMeanN231129.png',width=16,height=16,units='cm',res=300)
  hist(bm,breaks=round(sqrt(B)),col='blue',main='')
  # dev.off()
}