print('file: PointEstPoissonSampleMean.R')
print(date())
# Poisson: sample mean estimator 
set.seed(1953)
M = 1e4; print(c(M,'number of Monte-Carlo runs M'))   
n = 5; print(c(n,'sample size n'))
lambda1 = 2.7; print(c(lambda1,'true rate (mean number of events) = variance')) 
muest = numeric(M)
for(m in 1:M) muest[m]=mean(rpois(n,lambda=lambda1))
library(latex2exp)
# png('MCmeanPois161224.png',width=16,height=16,units='cm',res=300)
hist(muest,30,col='blue',xlab=TeX('$\\hat{\\lambda}$'),main='',las=1,cex.lab=1.5)
meanmuest = mean(muest)  # (expected: close to mu = 2.7)
varmuest = var(muest) # (expected: close to sigma^2/n=lambda/n=2.7/5=0.54)
sdmuest = sd(muest)   # (expected: sqrt(lambda/n)=0.73)
print(c(round(meanmuest,3),'meanmuest'))
print(c(round(sdmuest,3),'sdmuest'))
meanmuestr = round(meanmuest,3); varmuestr = round(varmuest,3); sdmuestr = round(sdmuest,3)
xt = 4.5
text(xt,1000,bquote(~mean(hat(lambda)) == .(meanmuestr)),col='blue',cex=1.5)
text(xt,800,bquote(~var(hat(lambda)) == .(varmuestr)),col='blue',cex=1.5)
text(xt,600,bquote(~sd(hat(lambda)) == .(sdmuestr)),col='blue',cex=1.5)
abline(v=lambda1,col='black',lty=1)
abline(v=meanmuest,col='blue',lty=4)
# dev.off()
# print(c('sqrt(lambda1/n) = ',sqrt(lambda1/n)))  # 0.7348469
print(c(round(sqrt(lambda1/n),3),'sd of sample means (CLT prediction)'))
# -----------------------------------------------------------------------------
# Results:
# "file: PointEstPoissonSampleMean.R"
# "Sun Dec 18 12:34:26 2022"
# "10000"   "number of Monte-Carlo runs M"
# "5"       "sample size n"
# "2.7"     "true rate (mean number of events) = variance"
# "2.703"   "meanmuest"
# "0.737"   "sdmuest"
# "0.735"   "sd of sample means (CLT prediction)"
# -----------------------------------------------------------------------------
# Remarks:
# The average of the sample means (2.703) is very close to the true mean rate (2.7).
# The standard deviation of the sample means (0.737) is very close to the prediction
#    from the Central Limit Theorem (0.735).
# -----------------------------------------------------------------------------

