print('file: MADnormal1.R')
# Estimate standard deviation: MAD,normal distribution
#      MAD = Median Absolute deviation about the Median
#      Moronna et al. (2006, p.~5)
set.seed(1953)
M = 1e3   # number of Monte-Carlo runs
n = 5      # sample size
sdest = numeric(M)
for(m in 1:M) {x=rnorm(n); sdest[m]=median(abs(x-median(x)))}
# png('MCmadNormal160819.png',width=16,height=16,units='cm',res=300)
hist(sdest,30,col='blue',xlab='MAD',main='',las=1,cex.lab=1.5)
meansdest = mean(sdest)  #   (expected: close to sigma = 1)
sdsdest = sd(sdest)
abline(v=1,col='black',lty=1)
abline(v=meansdest,col='blue',lty=4)
xt = 0.7
text(xt,65,pos=4,paste('mean of estimate = ',as.character(round(meansdest,3))),col='blue',cex=1.5)
text(xt,55,pos=4,paste('sd of estimate = ',as.character(round(sdsdest,3))),col='blue',cex=1.5)
# dev.off()