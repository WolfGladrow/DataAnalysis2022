print('file: ProbBernoulliDieEx.R')
# exact: f = p 
p=1/6; L=1000; narr=numeric(L); parrex=numeric(L) 
for(j in 1:L) {n = 6*j; narr[j] = n; parrex[j] = dbinom(j,n,p)}
# png('BernoulliDieEx200517.png',width=16,height=16,units='cm',res=300)
plot(narr,parrex,log='x',type='p',lwd=4,col='blue',xlab='n',
     ylab='Probability for f = p',las=0,cex=0.6,cex.lab=1.5)
# dev.off()