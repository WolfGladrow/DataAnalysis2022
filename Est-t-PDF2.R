print('file: Est-t-PDF2.R')
# estimate t distribution by Monte Carlo simulation based on Albert (2009)
set.seed(1953)
source('tstatisticFct.R')
m=10; n=10 # sample sizes
my.tsimulation=function() # generate random samples
  tstatisticFct(rnorm(m,mean=10,sd=2),rnorm(n,mean=10,sd=2)) 
tstat.vector=replicate(10000,my.tsimulation()) 
# png('MonteCarlo1e4-t-PDF160725a.png',width=16,height=16,units='cm',res=300)
plot(density(tstat.vector,from=-3.5,to=3.5),xlim=c(-3,3),ylim=c(0,0.4),
     xlab='t',lwd=4,main='',las=1,cex.lab=1.5)
curve(dt(x,df=18),col='red',add=TRUE,lty=2) # add plot of exact t PDF 
legend('bottom',c('MC','t(18)'),lwd=c(4,1),col=c('black','red'),lty=c(1,2),cex=1.5)
# dev.off()