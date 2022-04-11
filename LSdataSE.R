print('file: R_LSdataSE.R')
# generate & plot mean of artifical data (with respect to y) & +- standard error of the mean
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
ymean = numeric(Lx); for(k in 1:Lx) ymean[k] = mean(y[k,])  # mean y-values
ypse = beta0+beta*x+se; ymse = beta0+beta*x-se
yexact = beta0+beta*x
# png('SLFartifialSE220308.png',width=16,height=16,units='cm',res=300)
plot(x,ypse,type='l',lty=2,lwd=1,col='black',ylim=c(0,10),      # plot exact line plus SE
     xlim=c(0,max(x)), xlab='x',ylab='y',las=1,pch=24,cex.lab=1.5)
lines(x,ymse,col='black',lwd=1,lty=2)                           # plot exact line minus SE
lines(x,yexact,col='black',lwd=2)                               # plot exact line 
points(x,ymean,lwd=3,col='blue',cex=1,pch=24)                   # plot mean y-values
# dev.off()