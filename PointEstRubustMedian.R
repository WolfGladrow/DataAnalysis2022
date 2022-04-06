print('file: PointEstRubustMedian.R')
# Robust estimate of central tendency: median, normal distribution
set.seed(1953)
M = 1000   # number of Monte-Carlo runs
n = 5      # sample size
muest = numeric(M)
for(m in 1:M) muest[m]=median(rnorm(n))
# png('MCmedianNormal160819.png',width=16,height=16,units='cm',res=300)
hist(muest,30,col='blue',xlab='Median',main='',las=1,cex.lab=1.5)
meanmuest = mean(muest); print(c(round(meanmuest,3),'meanmuest'))  #  (expected: close to mu = 0)
varmuest = var(muest); print(c(round(varmuest,3),'varmuest'))
sdmuest = sd(muest); print(c(round(sdmuest,3),'sdmuest'))
text(-1.,80,'mean of estimate',col='blue',cex=1.5)
text(-1.1,70,paste('= ',as.character(round(meanmuest,3))),col='blue',cex=1.5)
text(0.9,80,'sd of estimate',col='blue',cex=1.5)
text(0.9,70,paste('= ',as.character(round(sdmuest,3))),col='blue',cex=1.5)
abline(v=0,col='black',lty=1)
abline(v=meanmuest,col='blue',lty=4)
# dev.off()