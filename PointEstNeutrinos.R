print('file: PointEstNeutrinos.R')
print(date())
# Poisson: neutrinos 
ja=seq(0,9)  # number of events (neutrinos in 10 s intervals)
frequencies=c(1042,860,307,78,15,3,0,0,0,1) # frequencies
lambdaEst=sum(ja*frequencies)/sum(frequencies) # estimate of mean rate
pPredict=dpois(ja,lambdaEst) # probabilities based on estimated lambda
relfre=frequencies/sum(frequencies) # relative frequencies
fPredict=pPredict*sum(frequencies)  # predicted frequencies
# ---------- plots:
sflag = 1
if (sflag == 1) {
  # png('NeutrinosF170720.png',width=16,height=12,units='cm',res=300)
  plot(ja,frequencies,type='p',col='red',xlab='Number of events j',ylab='',
       main='',lwd=3,cex=0.5,las=1,cex.lab=1.5)
  title(ylab=expression(paste('Frequencies ',f[j])),line=2.3,cex.lab=1.5)
  points(ja,fPredict,col='blue',lwd=2,cex=0.5)
  text(6.4,800,pos=4,expression(paste(hat(lambda))),col='red',cex=1.5)
  text(6.8,800,pos=4,paste('= ',as.character(round(lambdaEst,3))),col='red',cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  # png('NeutrinosP170720.png',width=16,height=12,units='cm',res=300)
  plot(ja,relfre,type='p',col='red',las=1,cex=0.5,main='',xlab='Number of events j',
       ylab='',lwd=3,cex.lab=1.5)
  title(ylab=expression(paste('Probabilities,relative frequencies ',p[j])),line=2.3,
        cex.lab=1.5)
  points(ja,pPredict,col='blue',lwd=2,cex=0.5)
  text(5.6,0.2,pos=4,expression(paste(hat(lambda))),col='red',cex=1.5)
  text(6.0,0.2,pos=4,paste('= ',as.character(round(lambdaEst,3))),col='red',cex=1.5)
  # dev.off()
}
if (sflag == 3) { # log(p_j)
  # png('NeutrinosLog10P170720.png',width=16,height=12,units='cm',res=300)
  plot(ja,log10(relfre),type='p',col='red',las=1,cex=0.5,main='',xlab='Number of events j',
       ylab='',lwd=3,ylim=c(-7,0),cex.lab=1.5)
  title(ylab=expression(paste('log( Probabilities,relative frequencies ',p[j],' )')),
        line=2.0,cex.lab=1.5)
  points(ja,log10(pPredict),col='blue',lwd=2,cex=0.5)
  text(5.6,-1,pos=4,expression(paste(hat(lambda))),col='red',cex=1.5)
  text(6.0,-1,pos=4,paste('= ',as.character(round(lambdaEst,3))),col='red',cex=1.5)
  # dev.off()
}
# ---------------------------------------------------
# Results: 
TNOC = sum(frequencies)        # total number of cases
TNOD = sum(ja*frequencies)     # total number of deaths
print(c('total number of cases  = ',TNOC))
print(c('total number of deaths = ',TNOD))
print(c('estimate of mean rate  = ',lambdaEst))
print(c('probabilities based on estimated lambda  = ',pPredict))
print(c('probabilities based on estimated lambda  = ',round(pPredict,4)))
print(c('relative frequencies  = ',round(relfre,4)))
print(c('predicted frequencies = ',round(fPredict,4)))
# -----------
ja1=seq(0,8)  # number of events (neutrinos in 10 s intervals)
frequencies1=c(1042,860,307,78,15,3,0,0,0) # frequencies
lambdaEst1=sum(ja1*frequencies1)/sum(frequencies1)
print(c('estimate of mean rate  = ',lambdaEst1)) # 0.77353579175705
