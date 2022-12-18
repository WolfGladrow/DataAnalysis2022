print('file: PointEstMeanF.R')
print(date())
# Estimate mean from random samples of F distribution
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
n = 5      # sample size
nu1 = 15; nu2 = 5  # degrees of freedom
muest = numeric(M)
expectedmean = nu2/(nu2-2); print(c(round(expectedmean,4),'expectedmean'))  # 5/3 = 1.667 for nu2 > 2
expectedvar = 2*nu2^2*(nu1+nu2-2)/(nu1*(nu2-2)^2*(nu2-4))
print(c(round(expectedvar,4),'expectedvar'))
expectedvarofmean = expectedvar/n
print(c(round(expectedvarofmean,4),'expectedvarofmean'))  # 1.333
for(m in 1:M) muest[m]=mean(rf(n,df1=nu1,df2=nu2))
library(latex2exp)
# png('MCmeanF160228.png',width=16,height=16,units='cm',res=300)
hist(muest,30,col='blue',xlab=TeX('$\\hat{\\mu}$'),main='',las=1,cex.lab=1.5)
meanmuest = mean(muest); print(c(round(meanmuest,4),'meanmuest'))  # (expected: close to mu = 1.667)
varmuest = var(muest); print(c(round(varmuest,4),'varmuest'))    # (expected: close to sigma^2/n = 1.334)
sdmuest  = sd(muest); print(c(round(sdmuest,4),'sdmuest'))     # (expected: close to sigma/sqrt(n) = 1.15)
meanmuestr = round(meanmuest,4); varmuestr = round(varmuest,3); sdmuestr = round(sdmuest,3)
xt = 6
text(xt,300,bquote(~mean(hat(mu)) == .(meanmuestr)),col='blue',cex=1.5)
text(xt,250,bquote(~var(hat(mu)) == .(varmuestr)),col='blue',cex=1.5)
text(xt,200,bquote(~sd(hat(mu)) == .(sdmuestr)),col='blue',cex=1.5)
abline(v=5/3,col='black',lty=1)
abline(v=meanmuest,col='blue',lty=4)
# dev.off()
# -----------------------------------------------------------------------------
# Results:
# "file: PointEstMeanF.R"
# "Sun Dec 18 11:22:01 2022"
# "1.6667"     "expectedmean"
# "6.6667"     "expectedvar"
# "1.3333"     "expectedvarofmean"
# "1.6419"     "meanmuest"
# "0.8764"     "varmuest"
# "0.9362"     "sdmuest"
# -----------------------------------------------------------------------------
# Remarks:
#  Although the estimate of the mean of the F-distribution is close to the true
#    mean (1.642 versus 1.667), the estimates of variance and standard deviation
#    are far away from the true values. Small samples (here: n = 5) are not 
#    able to extract the large variance that 'is located in the right tail'.
# -----------------------------------------------------------------------------
