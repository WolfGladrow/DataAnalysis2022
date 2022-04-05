print('file: PDsPDFsUniformDiscrete6.R')
# plot discrete uniform distribution for die
N = 6; xp = seq(1,N); yp =numeric(N) + 1/N
# png('UnifDiscrete160817.png',width=16,height=16,units='cm',res=300)
plot(xp,yp,type='p',lwd=4,col='blue',xlab='k',
     ylab='Probability',las=1,cex=0.6,ylim=c(0,0.2))
# dev.off()