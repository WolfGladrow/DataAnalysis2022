print('file: PDsPDFsHypergeoPDexample.R')
# hypergeometric distribution
N = 5   # total number of balls
W = 4   # number of white balls
B = N-W # number of black balls
J = 3   # number of balls drawn from the urn 
karr = seq(0,J); xk = seq(-1,J+1,0.01)
# dhyper(x, m, n, k, log = FALSE)
# m	the number of white balls in the urn.
# n	the number of black balls in the urn.
# k	 the number of balls drawn from the urn.
yPD = dhyper(x=karr,m=W,n=B,k=J)
# png('Hypergeo160817.png',width=16,height=12,units='cm',res=300)
plot(karr,yPD,type='p',lwd=3,col='blue',xlab='k',
     ylab='Hypergeometric probabilities',cex=0.4,las=1)
# dev.off()