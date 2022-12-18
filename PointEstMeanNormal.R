print('file: PointEstMeanNormal.R')
print(date())
# Estimate mean (mu) from random sample of normal distribution
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
n = 5      # sample size
muest = numeric(M)
for(m in 1:M) muest[m]=mean(rnorm(n))
# install.packages('latex2exp') 
library(latex2exp)
# png('MCmeanNormal160227.png',width=16,height=16,units='cm',res=300)
hist(muest,30,col='blue',xlab=NA,main='',las=1,cex.lab=1.5)
title(xlab=TeX('$\\hat{\\mu} = \\bar{x} = \\frac{1}{n} \\sum_j x_j$'),line=4.3,cex.lab=1.5)
meanmuest = mean(muest); print(c(round(meanmuest,4),'meanmuest'))  # -0.02700781 (expected: close to mu = 0)
varmuest = var(muest); print(c(round(varmuest,4),'varmuest'))  #  0.1857987 (expected: close to sigma^2/n = 1/5 = 0.2)
sdmuest = sd(muest); print(c(round(sdmuest,4),'sdmuest'))      # 0.4310438
meanmuestr = round(meanmuest,3); varmuestr = round(varmuest,3); sdmuestr = round(sdmuest,3)
text(0.8,90,bquote(~mean(hat(mu)) == .(meanmuestr)),col='blue',cex=1.5)
text(0.8,80,bquote(~var(hat(mu)) == .(varmuestr)),col='blue',cex=1.5)
text(0.8,70,bquote(~sd(hat(mu)) == .(sdmuestr)),col='blue',cex=1.5)
abline(v=0,col='black',lty=1)
abline(v=meanmuest,col='blue',lty=4)
# dev.off()
# -----------------------------------------------------------------------------
# Results:
# "file: PointEstMeanNormal.R"
# "Sun Dec 18 11:37:40 2022"
# "-0.027"    "meanmuest"
#  "0.1858"   "varmuest"
#  "0.431"    "sdmuest"
# -----------------------------------------------------------------------------
# Remarks:
# The average sample mean (-0.027) is close to the true mean (0) which is no 
#   surprise. The standard deviation of the sample means (0.431) is relatively 
#   large and close to the value 1/sqrt(n) = 1/sqrt(5) = 0.447 predicted by
#   the Central Limit Theorem (CLT).
# -----------------------------------------------------------------------------
