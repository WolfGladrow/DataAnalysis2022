print('file: ProbBernoulliDie600nDnormal.R')
print(date())
p=1/6; n=600; karr=seq(0,n); parr=numeric(n+1) 
parr = dbinom(karr,n,p)
# normal/Gaussian envelope
muB = n*p
sdB = sqrt(n*p*(1-p)); print(c(round(sdB,2),'sdB'))
varB = n*p*(1-p); print(c(round(varB,2),'varB'))
k1 = 81; k2 = 121
x = seq(k1,k2,0.1)
y = dnorm(x,muB,sdB)
# png('BernoulliDie600nDnormal200517.png',width=16,height=16,units='cm',res=300)
plot(karr[k1:k2],parr[k1:k2],type='p',lwd=4,col='blue',xlab='k',
       ylab='B(k; n=600, p=1/6)',las=1,cex=0.6,ylim=c(0,max(parr)),cex.lab=1.5)
lines(x,y,col='black',lty=3)
# dev.off()
# -----------------------------------------------------------------------------
# Results:
# "file: ProbBernoulliDie600nDnormal.R"
# "Sun Dec 18 14:16:53 2022"
#  "9.13" "sdB" 
# "83.33" "varB"
# -----------------------------------------------------------------------------
