print('file: NeutrinosData.R')
k=seq(0,9)  # number of events (neutrinos in 10 s intervals)
frequencies=c(1042,860,307,78,15,3,0,0,0,1)
# png('NeutrinosF220328.png',width=16,height=16,units='cm',res=300)
plot(k,frequencies,type='p',col='black',xlab='Number of events',ylab='Frequency',
       main='',lwd=4,cex=0.6,las=1,cex.lab=1.5)
# dev.off()