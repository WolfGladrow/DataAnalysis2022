print('file: PDsPDFsUniformDiscrete6.R')
# plot discrete uniform distribution for die
N = 6; xp = seq(1,N); yp =numeric(N) + 1/N
# png('UnifDiscrete160817.png',width=16,height=12,units='cm',res=300)
plot(xp,yp,type='p',lwd=3,col='blue',xlab='k',
     ylab='Probabilities discrete uniform PD',las=1,cex=0.5,ylim=c(0,0.2))
# dev.off()