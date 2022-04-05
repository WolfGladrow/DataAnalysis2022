print('file: SolutionPoissonBinomial.R')
# EXERCISE & SOLUTION: compare binomial & Poisson distribution
n = 10; k = seq(0,n); p = 0.27; pk = dbinom(x=k,size=n,prob=p)
lambda1 = sum(k*pk); lambda1r = round(lambda1,3)
pkp = dpois(k,lambda=lambda1)
# png('BinomPoisson160817.png',width=16,height=16,units='cm',res=300)
plot(k,pk,type='p',lwd=4,col='black',xlab='k',ylim=c(0,max(c(pkp,pk))),
     ylab='Probabilities',las=1,cex=0.6,cex.lab=1.5)
points(k,pkp,col='magenta',lwd=3,cex=0.6,pch=24)
text(7,0.25,bquote(~p == .(p)),col='black',cex=1.5)
text(7,0.2,bquote(~lambda == .(lambda1r)),col='magenta',cex=1.5)
# dev.off()