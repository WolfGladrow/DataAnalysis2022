print('file: PoissonRegrData.R')
# Poisson regression: generate artificial data set 
# (1) exact model: log(mu) = beta0 + beta*x;  -1 <= x <= +1
beta0 = 1.3; beta = 1.2; xmin = -1; xmax = 1
n = 30                 # sample size
dx = (xmax-xmin)/(n-1); x = round(seq(xmin,xmax,dx),3)
set.seed(1953)         # set seed for random number generators
logmu = beta0+beta*x
mu = exp(beta0+beta*x)
y = numeric(n)
for(k in 1:n) y[k] = rpois(1,lambda=mu[k])
# (2) plot data:
# png('PoissonRegArtData160929.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='p',lwd=4,col='blue',xlab='x',ylab='y',las=1,cex=0.6,cex.lab=1.5)
# dev.off()