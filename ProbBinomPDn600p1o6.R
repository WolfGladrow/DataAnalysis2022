print('file: ProbBinomPDn600p1o6.R')
# dbinom(k;n=600,p=1/6
p=1/6; n=600; karr=seq(0,n); parr=numeric(n+1) 
parr = dbinom(karr,n,p)
# png('BernoulliDie600n200517.png',width=16,height=16,units='cm',res=300)
plot(karr,parr,type='p',lwd=4,col='blue',xlab='k',
     ylab='B(k; n=600, p=1/6)',las=1,cex=0.6,cex.lab=1.5)
# dev.off()