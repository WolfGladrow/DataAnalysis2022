print('file: LSartificialSpline.R')
# generate & plot artificial data
beta0 = 8; beta = -1.2 # true intercept & slope
sigma = 1              # true standard deviation of normal noise
# ----------- generate data (begin)
set.seed(1953)                       # set seed for random number generators
x = seq(1,5,0.25); Lx = length(x)    # non-stochastic (uncertainty = 0)
Lr = 10                              # number of data for each x-value ('replicates')
se = sigma/sqrt(Lr); print(c(round(se,4),'standard error'))
y = matrix(data=NA,nrow=Lx,ncol=Lr)  # Lx x Lr = 17 x 10 matrix
for(k in 1:Lx) y[k,] = beta0+beta*x[k]+sigma*rnorm(Lr) # additive normal noise
# ----------- generate data (end)
ymean = numeric(Lx)
for(k in 1:Lx) ymean[k] = mean(y[k,])
# png('SLFartificialSpline220308.png',width=16,height=16,units='cm',res=300)
plot(x,y[,1],type='p',lwd=4,col='black',cex=0.6,ylim=c(0,10),     # plot 1. sample
             xlim=c(0,max(x)),xlab='x',ylab='y',las=1,cex.lab=1.5)
for(k in 2:Lr) points(x,y[,k],lwd=4,col='black',cex=0.6)          # plot replicates
lines(spline(x,ymean),col='blue')  # <---- spline()
points(x,ymean,lwd=3,col='blue',cex=1,pch=24)
xn = seq(-3,3,0.01); yn = dnorm(xn,mean=0,sd=sigma)
lines(-yn+x[1],xn+beta0+beta*x[1],col='black')
lines(-yn+x[Lx],xn+beta0+beta*x[Lx],col='black')
# dev.off()
# -----------------------------------------------------------------------------
# Remarks:
# The R routine spline() performs cubic (or Hermite) spline interpolation of given data 
#   points, returning a list of points obtained by the interpolation.
# Compare also splinefun(): it returns a function performing the interpolation
# -----------------------------------------------------------------------------