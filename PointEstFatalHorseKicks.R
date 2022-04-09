print('file: PointEstFatalHorseKicks.R')
# fatal horse kicks (Barlow, 1999) 
ja=seq(0,4)                    # number of events
frequencies=c(109,65,22,3,1)   # frequencies
lambdaEst=sum(ja*frequencies)/sum(frequencies) # estimate of mean number of deaths per corps per year
# Bayesian estimate:
n = sum(frequencies)
s = sum(ja*frequencies)
lambdaEstB = (s+1)/n; print(c(round(lambdaEstB,4),'lambdaEstB'))
ulambdaEstB = sqrt((s+1))/n; print(c(round(ulambdaEstB,4),'ulambdaEstB'))
lambdaEstBr = round(lambdaEstB,3); ulambdaEstBr = round(ulambdaEstB,3)
# prediction based on Poisson distribution:
pPredict=dpois(ja,lambdaEstB) # probabilities based on estimated lambda
relfre=frequencies/sum(frequencies) # relative frequencies
fPredict=pPredict*sum(frequencies)  # predicted frequencies
# ---------- plots:
sflag = 1
if (sflag == 1) {
  # png('FatalHorseKicksF170720.png',width=16,height=16,units='cm',res=300)
  plot(ja,frequencies,type='p',col='black',xlab='Number of events j',ylab='',
       main='',lwd=4,cex=0.6,las=1,cex.lab=1.5)
  title(ylab=expression(paste('Frequencies ',f[j])),line=2.3,cex.lab=1.5)
  points(ja,fPredict,col='magenta',lwd=4,cex=0.6,pch=24)
  xt = 2
  text(2,80,pos=4,bquote(~bar(x) == .(lambdaEst)),col='magenta',cex=1.5)
  text(2,100,pos=4,bquote(~hat(lambda) == .(lambdaEstBr)  %+-% .(ulambdaEstBr)),col='magenta',cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  # png('FatalHorseKicksP170720.png',width=16,height=16,units='cm',res=300)
  plot(ja,relfre,type='p',col='black',las=1,cex=0.6,main='',xlab='Number of events j',
       ylab='',lwd=4,cex.lab=1.5)
  title(ylab='Probability, relative frequency',line=2.5,cex.lab=1.5)
  points(ja,pPredict,col='magenta',lwd=4,cex=0.6,pch=24)
  xt = 2
  text(2,0.4,pos=4,bquote(~bar(x) == .(lambdaEst)),col='magenta',cex=1.5)
  text(2,0.5,pos=4,bquote(~hat(lambda) == .(lambdaEstBr)  %+-% .(ulambdaEstBr)),col='magenta',cex=1.5)
  # dev.off()
}