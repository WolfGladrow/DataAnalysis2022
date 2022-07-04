print('File: QuantileRTricks.R')
# demonstrate quantile routine
set.seed(1953) # set seed for random number generators
M = 1e3 # number of Monte Carlo runs
x = rnorm(M)
alpha = 1-0.95 # 95% interval
q = quantile(x,c(alpha/2,1-alpha/2))
qa1 = qnorm(alpha/2)
qa2 = qnorm(1-alpha/2)
print(c(round(as.numeric(q[1]),4),'q[1] from random sampling'))
print(c(round(qa1,4),'qa1 analytic'))
print(c(round(as.numeric(q[2]),4),'q[2] from random sampling'))
print(c(round(qa2,4),'qa2 analytic'))
# png('QuantileRoutine210807.png',width=16,height=12,units='cm',res=300)
hist(x,breaks=30,col='blue',main='',las=1,xlab='x',cex.lab=1.5)
abline(v=q[1],col='magenta',lty=3)
abline(v=q[2],col='magenta',lty=3)
abline(v=qa1,col='black',lty=4)
abline(v=qa2,col='black',lty=4)
# dev.off()