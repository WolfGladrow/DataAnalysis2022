print('file: CLTdeviationsTails.R')
# Central Limit Theorem: deviations in tails
set.seed(1953) # set seed for random number generators
N = 1e4     # number of random numbers to sum up 
M = 1e5     # number of sums (= Monte Carlo runs)
s = numeric(M)    # creates a vector of zeros of length M
for(k in 1:M) s[k] = sum(runif(N))  # short version
out1 = density(s,from=4875,to=5125)  # yields n=512 paired estimates (out1$x,out1$y) for the PDF of s
xest = out1$x; yest = out1$y
varud = 1/12; print(c(round(varud,4),'varud'))        # variance of uniform distribution
varsN = N*varud; print(c(round(varsN,4),'varsN'))     # expected variance of sums of N random numbers
stdsN = sqrt(varsN); print(c(round(stdsN,4),'stdsN')) # expected standard deviation of sums of N random numbers
ynorm = dnorm(xest,mean=N/2,sd=stdsN)  # theoretical distribution
r = yest/ynorm
library(latex2exp)
# png('CLTtails160722.png',width=16,height=16,units='cm',res=300)
plot(xest,r,type='l',lwd=3,col='blue',xlab='Sum over 10000 random numbers',cex.lab=1.5,
     ylab='Estimated density/normal PDF',main='',las=1,xlim=c(4900,5100),ylim=c(0.9,1.6))
abline(1,0,col='black')
text(4900,1.4,TeX('$M = 10^5$'),pos=4,col='blue',cex=1.5)
# dev.off()