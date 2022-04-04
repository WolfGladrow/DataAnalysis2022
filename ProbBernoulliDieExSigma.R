print('file: ProbBernoulliDieExSigma.R')
# exact: f = p & upper limit 
p=1/6; L=1000; narr=numeric(L); parrex=numeric(L)
for(j in 1:L) {n = 6*j; narr[j] = n; parrex[j] = dbinom(j,n,p)}
sigarr=sqrt(narr*p*(1-p))
c1 = 1/sqrt(2*pi)
# png('BernoulliDieExSigma200517.png',width=16,height=16,units='cm',res=300)
plot(narr,parrex,log='x',type='p',lwd=4,col='magenta',xlab='n',
     ylab='Probability for f = p',las=1,cex=0.6,ylim=c(0,0.45),cex.lab=1.5)
points(narr,c1/sigarr,col='black',lty=3,lwd=4,pch=24,cex=0.6)
# dev.off()