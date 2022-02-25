print('file: PDsPDFsBrokenStickPDexample.R')
# broken stick distribution
n = 10; karr = seq(1,n); pk = seq(1,n); kinv = 1/karr;
for(k in 1:n) pk[k]=sum(kinv[k:n])/n
# png('BrokenStick160817.png',width=16,height=12,units='cm',res=300)
plot(karr,pk,type='p',lwd=3,col='blue',xlab='k',
     ylab='Broken stick PD',las=1,cex=0.4)
# dev.off()