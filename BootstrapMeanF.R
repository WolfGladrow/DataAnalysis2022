print('file: BootstrapMeanF.R')
# estimate uncertainty of mean from an F distribution via bootstrapping
n = 30 # sample size
set.seed(1953)
nu1=15; nu2=3 # degrees of freedom
meanA = nu2/(nu2-1) # mean (for nu2 > 2) 
print(c(meanA,'mean (analytic)'))
r = rf(n,df1=nu1,df2=nu2) # sample from F PDF
B = 1e4 # number of bootstrap samples
xrange = seq(1,n)
bm = numeric(B)
for(j in 1:B) {
  i = sample(x=xrange,n,replace=TRUE) # sampling with replacement
  bm[j] = mean(r[i])                  # calculate mean value of resample
}
print(c(round(mean(bm),4),'mean(xm)'))
print(c(round(median(bm),4),'median(xm)')) # robust estimate of central tendency
# 95% confidence interval:
q = as.numeric(quantile(bm,c(0.05/2,1-0.05/2)))
print(c(round(q,4),'95% confidence interval (bootstrap)'))
# library(latex2exp)
sflag = 1
if (sflag == 1) {
  # png('BootstrapMeanF231128.png',width=16,height=16,units='cm',res=300)
  hist(bm,breaks=round(sqrt(B)),col='blue',main='')
  # dev.off()
}