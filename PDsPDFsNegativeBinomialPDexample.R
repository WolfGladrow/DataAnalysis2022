print('file: PDsPDFsNegativeBinomialPDexample.R')
# negative binomial distribution 
ka = seq(0,30)   # number of failures before sth success occurs
s = 5            # (chosen) number of successes: integer > 0 or s = 1,2,3, ...
p = 0.3          # probability of success in single Bernoulli trial
pNBD = dnbinom(ka,s,p)  # sum(pNBD)
# png('NegativeBinomial180717.png',width=16,height=12,units='cm',res=300)
plot(ka,pNBD,type='p',lwd=3,col='blue',xlab='Number of successes, k',
     ylab='Negative binomial distribution',las=1,cex=0.4)
text(5,0.02,paste('s = ',as.character(s)),col='black',pos=4)
text(5,0.01,paste('p = ',as.character(p)),col='black',pos=4)
# dev.off()
