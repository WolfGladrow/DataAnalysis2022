print('file: LowerIncompleteGamma.R')
# lower incomplete gamma functions
s = 5; xa = seq(0,10,0.1); ILga = pgamma(xa,s) 
# png('IncompleteGamma160810.png',width=16,height=16,units='cm',res=300)
plot(xa,ILga,type='l',lwd=3,col='black',xlab='x',
     ylab='Incomplete gamma function',las=1,cex.lab=1.5)
text(0,0.8,bquote(~s == .(s)),col='black',cex=1.5,pos=4)
# dev.off()