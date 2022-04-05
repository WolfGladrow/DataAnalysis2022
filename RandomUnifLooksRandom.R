print('file: RandomUnifLooksRandom.R')
# uniform random number look randomly distributed (Robert & Casella, 2009, p.43)
set.seed(1953) # set seed for random number generators
Nsim=10^4     # number of random numbers
x=runif(Nsim)
x1=x[-Nsim]   # vectors to plot
x2=x[-1]      #    adjacent pairs
library(latex2exp)
# png('runif160811.png',width=16,height=16,units='cm',res=300)
par(mfrow=c(1,3))
hist(x,col='blue',cex.lab=1.5)       # histgram
plot(x1,x2,col='blue',cex.lab=1.5,xlab=TeX('$x_1$'),ylab=TeX('$x_2$'))   # scatterplot
acf(x,col='blue',cex.lab=1.5)        # auto-correlation function
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# How to interpret 'x1=x[-Nsim]' & 'x2=x[-1]'? Example with small sample size (Nsim=6)
Nsim = 6; xs = runif(Nsim); x1s=xs[-Nsim]; x2s=xs[-1]
xs  # 0.8154753 0.9895101 0.4506347 0.1759375 0.4960116 0.5183193
x1s # 0.8154753 0.9895101 0.4506347 0.1759375 0.4960116
x2s #           0.9895101 0.4506347 0.1759375 0.4960116 0.5183193
# x1=x[-Nsim] = all x except the one with index Nsim (here: last)
# x2=x[-1]    = all x except the one with index 1 (here: first)
# ----------------------------------------------------------------