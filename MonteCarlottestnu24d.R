print('file: MonteCarlottestnu24d.R')
# Monte Carlo simulation: Student-t(t; nu=24); density estimation
n = 25 # sample size
nu = n-1 # degrees of freedom
mu = 0 # true mean
sigma = 1.5 # true standard deviation (arbitrary value > 0)
set.seed(1953) # seed for random number generators
M = 1e5; sflag = 2 # number of Monte Carlo runs for density estimate
tvalue = numeric(M)
for(j in 1:M) {
    r = rnorm(n,mu,sigma) # n random values from normal distribution
    rm = mean(r) # sample mean
    rsd = sd(r)  # sample standard deviation
    tvalue[j] = (rm-mu)/(rsd/sqrt(n))
}
if (sflag == 2) { # density estimate
  # png('MonteCarlo-t-density220403.png',width=16,height=16,units='cm',res=300)
  plot(density(tvalue,from=-4,to=4),type='l',lwd=4,col='black',xlab='x',
       ylab='Density',las=1,cex.lab=1.5,main='',xaxs='i') #,yaxs='i')
  xp = seq(-4,4,0.01); yp = dt(xp,nu)
  lines(xp,yp,col='magenta',lty=4,lwd=2)
  text(-2.5,0.35,bquote(~nu == .(nu)),col='black',cex=1.5,pos=4)
  legend('bottom',legend=c('Monte Carlo','Student-t'),col=c('black','magenta'),lty=c(1,4),
         lwd=c(4,2),cex=1.5)
  # dev.off()
}
# ------------------------------------------------------------------
