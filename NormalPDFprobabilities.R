print('file: NormalPDFprobabilities.R')
# normal PDF, probabilities by integration
dx=0.01; x = seq(-4,4,dx); y = dnorm(x)
x1=-4; x2=0; y1=dnorm(x1); y2=dnorm(x2); 
xn=seq(x1,x2,dx); yn=dnorm(xn); xf=c(x2,x1,xn); yf=c(0,0,yn) 
x3 = 1; x4 = 2; y3=dnorm(x3); y4=dnorm(x4); 
xn3=seq(x3,x4,dx); yn3=dnorm(xn3); xf3=c(x4,x3,xn3); yf3=c(0,0,yn3) 
P2 = integrate(dnorm,x3,x4)$value; P2r = round(P2,4)
# png('NormalPDFprobabilities220329.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='l',lwd=4,col='black',xlab='x',ylab='Normal PDF',las=1,cex=0.6,
     cex.lab=1.5,xlim=c(-3,3))
polygon(xf,yf,col='blue')
polygon(xf3,yf3,col='magenta')
lines(x,y,col='black',lwd=4)
text(-3,0.25,TeX('$P_1 = 1/2$'),col='blue',cex=1.5,pos=4)
text(1.2,0.25,bquote(~P[2] == .(P2r)),col='magenta',cex=1.5,pos=4)
# dev.off()
P2a = pnorm(x4) - pnorm(x3)
# -----------------------------------------------------------------------------
# Remarks:
# Here, the aim was to show that probabilities can be calculated by integration
#   over probability densities.
# The R routine pnorm() provides such integrals in fast way and, for example,
#   the probability for x3 = 1 <= x <= x4 = 2 can be calculated as follows:
#   pnorm(x4) - pnorm(x3)
# Actually, pnorm() provides the cumulative distribution function (CDF) for the
#   normal distribution.
# -----------------------------------------------------------------------------
