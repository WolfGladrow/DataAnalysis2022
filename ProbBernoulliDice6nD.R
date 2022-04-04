print('file: ProbBernoulliDice6n.R')
# Bernoulli dbinom(k;n=600,p=1/6) 
p=1/6; n=600; karr=seq(0,n); parr=numeric(n+1) 
parr = dbinom(karr,n,p)
k1 = 81; k2 = 121
# png('BernoulliDie600nD200517.png',width=16,height=16,units='cm',res=300)
plot(karr[k1:k2],parr[k1:k2],type='p',lwd=4,col='blue',xlab='k',
     ylab='B(k; n=600, p=1/6)',las=1,cex=0.6,ylim=c(0,max(parr)),cex.lab=1.5)
# dev.off()