file('print: NeutrinosMeanPoisson.R')
# neutrinos: calculate sample mean and compare relative frequencies with Poisson probabilities
k=seq(0,9)  # number of events (neutrinos in 10 s intervals)
frequencies=c(1042,860,307,78,15,3,0,0,0,1) # frequencies
lambdaEst=sum(k*frequencies)/sum(frequencies) # estimate of mean rate = E[k]
print(c(round(lambdaEst,3),'lambdaEst'))
rf = frequencies/sum(frequencies) # relative frequencies
lambdaEst2 = sum(k*rf); print(c(round(lambdaEst2,3),'lambdaEst2'))
lambdaEst3 = sum((k-lambdaEst2)^2*rf); print(c(round(lambdaEst3,3),'lambdaEst3'))
pPredict=dpois(k,lambdaEst) # probabilities based on estimated lambda
q = rf/pPredict
sflag = 2
if (sflag == 2) {
  # png('NeutrinosP2200328.png',width=16,height=16,units='cm',res=300)
  plot(k,rf,type='p',col='black',las=1,cex=0.6,main='',xlab='Number of events, k',
       ylab='',lwd=4,cex.lab=1.5,pch=20)
  title(ylab='Probability,relative frequency',line=2.5,cex.lab=1.5)
  points(k,pPredict,col='magenta',lwd=4,cex=0.6,pch=24)
  lambdar = round(lambdaEst,3)
  text(5.6,0.2,bquote(~hat(lambda) == .(lambdar)),col='magenta',pos=4,cex=1.5)
  # dev.off()
}