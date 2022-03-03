print('file: PointEstFatalHorseKicks.R')
# fatal horse kicks (Barlow, 1999) 
ja=seq(0,4)                    # number of events
frequencies=c(109,65,22,3,1)   # frequencies
lambdaEst=sum(ja*frequencies)/sum(frequencies) # estimate of mean number of deaths per corps per year
pPredict=dpois(ja,lambdaEst) # probabilities based on estimated lambda
relfre=frequencies/sum(frequencies) # relative frequencies
fPredict=pPredict*sum(frequencies)  # predicted frequencies
# ---------- plots:
sflag = 2
if (sflag == 1) {
  # png('FatalHorseKicksF170720.png',width=16,height=12,units='cm',res=300)
  plot(ja,frequencies,type='p',col='red',xlab='Number of events j',ylab='',
       main='',lwd=3,cex=0.5,las=1,cex.lab=1.5)
  title(ylab=expression(paste('Frequencies ',f[j])),line=2.3,cex.lab=1.5)
  points(ja,fPredict,col='blue',lwd=2,cex=0.5)
  text(0.61,40,pos=4,expression(paste(hat(lambda),' = 0.61')),col='red',cex=1.5)
  # dev.off()
}
if (sflag == 2) {
  png('FatalHorseKicksP170720.png',width=16,height=12,units='cm',res=300)
  plot(ja,relfre,type='p',col='red',las=1,cex=0.5,main='',xlab='Number of events j',
       ylab='',lwd=3,cex.lab=1.5)
  title(ylab='Probabilities,relative frequencies',line=2.5,cex.lab=1.5)
  points(ja,pPredict,col='blue',lwd=2,cex=0.5)
  text(0.61,0.2,pos=4,expression(paste(hat(lambda),' = 0.61')),col='red',cex=1.5)
  dev.off()
}