print('file: MonteCarlottestnu24.R')
# Monte Carlo simulation: Student-t(t; nu=24)
n = 25 # sample size
nu = n-1 # degrees of freedom
mu = 0 # true mean
sigma = 1.5 # true standard deviation (arbitrary value > 0)
set.seed(1953) # seed for random number generators
M = 1e3; sflag = 1 # number of Monte Carlo runs for histogram
tvalue = numeric(M)
for(j in 1:M) {
    r = rnorm(n,mu,sigma) # n random values from normal distribution
    rm = mean(r) # sample mean
    rsd = sd(r)  # sample standard deviation
    tvalue[j] = (rm-mu)/(rsd/sqrt(n))
}
if (sflag == 1) {
    mybreaks = round(sqrt(M))
    # png('MonteCarlo-t-hist220403.png',width=16,height=16,units='cm',res=300)
    hist(tvalue,breaks=mybreaks,col='blue',main='',xlab='t',las=1,cex.lab=1.5)
    abline(v=mu,col='black',lty=4)
    # dev.off()
}
# ------------------------------------------------------------------
