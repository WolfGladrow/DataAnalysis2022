print('file: PDsPDFsStudent-tMonteCarlo.R')
# Estimation of t distribution using Monte Carlo simulation
M = 1e5  # number of Monte Carlo runs
t = numeric(M)
n1 = 3; n2 = 2  # sample lengths (small -> large differences
# between normal & t distribution)
nu = n1 + n2 - 2  # degrees of freedom
set.seed(1953)  # set seed for random number generators
for (k in 1:M) { 
  x1 = rnorm(n1); x2 = rnorm(n2);  # sample from std. normal dist.
  x1mean = mean(x1); x2mean = mean(x2);
  SS1 = sum( (x1-x1mean)^2 );   # sum of squares
  SS2 = sum( (x2-x2mean)^2 );
  spsq = (SS1+SS2)/nu;
  ssqx1x2 = spsq/n1+spsq/n2;
  t[k] = (x1mean-x2mean)/sqrt(ssqx1x2)}
# png('MonteCarloStudentDist220227.png',width=16,height=16,units='cm',res=300)
plot(density(t,from=-3.5,to=3.5),col='blue',xlim=c(-3,3),ylim=c(0,0.4),
     xlab='t',ylab='Standard normal & t distribution',main='',lwd=2,las=1,cex.lab=1.5)
xp = seq(-4,4,0.01)
lines(xp,dt(xp,nu),col='black',lwd=3,lty=3)
lines(xp,dnorm(xp),col='magenta',lwd=1,lty=1)
legend('bottom',legend=c('normal','t','MC estimate'),col=c('magenta','blue','black'),
       lty=c(1,1,3),lwd=c(1,2,3),cex=1.5)
# dev.off()