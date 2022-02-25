print('file: RandomBeta2x5')
# random numbers from beta(2,5)
alpha = 2; beta = 5
dx = 0.01
x = seq(0,1,dx)
y = dbeta(x,alpha,beta)
print(' ---------------------------------------------------')
print('CDF (estimate) from discrete approximation of PDF by integration:')
L = length(x)
CDFapp = numeric(L)
for(i in 2:L) CDFapp[i] = CDFapp[i-1]+(y[i-1]+y[i])*dx/2
CDFana = pbeta(x,alpha,beta)
print(' ---------------------------------------------------')
print('Inverse CDF:')
CDFinvA = qbeta(CDFapp,alpha,beta)
print(' ---------------------------------------------------')
print('Generate random numbers from beta(a,b)')
# How to generate random numbers from beta(a,b)?
# (1) Generate random numbers from uniform PDF ([0,1])
# (2) Find corresponding x values from numerical CDF
set.seed(1953) # set seed for random number generators
M = 1e4 # number of Monte Carlo runs
r = runif(M) 
rb = numeric(M)
for(i in 1:M) {j=which.min((CDFapp-r[i])^2); rb[i] = x[j]}
par(mfrow=c(1,1))
# png('RandomDiscrete210427all.png',width=16,height=12,units='cm',res=300)
  par(mfrow=c(2,2))
  plot(x,y,type='l',lwd=3,col='blue',xlab='x',ylab='Beta(x;2,5)',las=1,cex=0.4)
  plot(x,CDFapp,type='p',lwd=3,col='blue',xlab='x',ylab='CDF(x)',las=1,cex=0.4)
  lines(x,CDFana,col='green')
  plot(CDFapp,x,type='p',lwd=3,col='blue',xlab='CDF(x)',ylab='Inverse CDF',las=1,cex=0.4)
  lines(CDFapp,CDFinvA,col='green')
  plot(density(rb,from=0,to=1),type='l',lwd=3,col='blue',xlab='x',
       ylab='Density',las=1,cex=0.4,main='')
  lines(x,y,col='green')
# dev.off()