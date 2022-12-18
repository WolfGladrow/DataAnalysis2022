print('file: RNasymTriangle.R')
# random numbers from asymmetric triangular PDF: mode = 0.9 
# install.packages('triangle')
library(triangle)
set.seed(1953) 
thetaMode = 0.9 # true mode
nonsymtri = function(x,thetaMode) {
  # nonsymmetric triangular PDF
  # 0 < x, thetaMode < 1
  # Dieter.Wolf-Gladrow@awi.de 6/2021
  q = 2*x/thetaMode
  L = length(x)
  for(i in 1:L) if(x[i] > thetaMode) q[i] = 2*(1-x[i])/(1-thetaMode)
  return(q)
}
sflag = 1
if (sflag == 1) {
M = 1e3 # number of Monte Carlo runs
x = rtriangle(M,0,1,thetaMode)
# png('TriPDFnonsym0d9M1e3R210614.png',width=16,height=16,units='cm',res=300)   # 1e3
plot(density(x,from=0,to=1),type='l',lwd=3,col='blue',xlab='x',
     ylab='Density',las=1,cex.lab=1.5,main='',ylim=c(0,2))
text(0.2,1.5,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
dx = 0.001; xp = seq(dx,1-dx,dx); yp = nonsymtri(xp,thetaMode)
lines(xp,yp,col='black')
# dev.off()
}
if (sflag == 2) {
M = 1e5
x = rtriangle(M,0,1,thetaMode)
# png('TriPDFnonsym0d9M1e5R210614.png',width=16,height=12,units='cm',res=300)   # 1e5
plot(density(x,from=0,to=1),type='l',lwd=3,col='blue',xlab='x',
     ylab='Density',las=1,cex.lab=1.5,main='',ylim=c(0,2))
text(0.2,1.5,paste('M = ',as.character(M)),col='blue',pos=4,cex=1.5)
dx = 0.001; xp = seq(dx,1-dx,dx); yp = nonsymtri(xp,thetaMode)
lines(xp,yp,col='black')
# dev.off()
}