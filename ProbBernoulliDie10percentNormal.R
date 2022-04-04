print('file: ProbBernoulliDie10percentNormal.R')
# n*f = n*p +- 10%, normal approximation
p=1/6; L=6000; narr=numeric(L); parrex=numeric(L)
parr10=numeric(L)
for(j in 1:5) {narr[j] = NA; parr10[j] = NA}
for(j in 6:L) {
  n = j;
  narr[j] = n; parrex[j] = dbinom(j,n,p)
  j1 = round(n/6 - n/60)
  j2 = round(n/6 + n/60)
  jr = seq(j1,j2)
  parr10[j] = sum(dbinom(jr,n,p))
}
library(latex2exp)
n = seq(1,6000)
x = n*p/10/sqrt(n*p*(1-p)) # How many sigmas for +10%?
pr = pnorm(x) - pnorm(-x)  # normal estimate
# png('BernoulliDie10percentNormal200517.png',width=16,height=16,units='cm',res=300)
plot(narr,parr10,log='x',type='p',lwd=3,col='blue',xlab='n',
     ylab=NA,las=1,cex=0.4,ylim=c(0,1),cex.lab=1.5)
title(ylab=TeX('$Probability\\, for\\, f = p \\pm 10%$'),line=2.5,cex.lab=1.5)
points(n,pr,lwd=3,col='black',cex=0.4)
# dev.off()