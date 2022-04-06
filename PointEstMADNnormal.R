print('file: PointEstMADNnormal.R')
# Estimate spread: MADN,normal distribution
#      MADN = Median Absolute deviation about the Median, Normalized
#      Moronna et al. (2006, p.~5)
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
n = 5      # sample size
sdest = numeric(M)
for(m in 1:M) {x=rnorm(n); sdest[m]=median(abs(x-median(x)))/0.6745}
# png('MCmadnNormal160819.png',width=16,height=16,units='cm',res=300)
hist(sdest,30,col='blue',xlab='MADN',main='',las=1)
meansdest = mean(sdest); print(c(round(meansdest,3),'meansdest'))  # (expected: close to sigma = 1)
sdsdest = sd(sdest); print(c(round(sdsdest,3),'sdsdest'))
abline(v=1,col='black',lty=1)
abline(v=meansdest,col='blue',lty=4)
xt = 2
text(xt,75,paste('mean of estimate = ',as.character(round(meansdest,3))),col='blue',cex=1.5)
text(xt,65,paste('sd of estimate = ',as.character(round(sdsdest,3))),col='blue',cex=1.5)
# dev.off()