print('file: RandomTentDensityEstimate')
# estimate density from 1e5 random numbers from tent PDF (7/2016)')
set.seed(1953)
M = 1e5; r = runif(M);  # uniformly distributed random numbers
y = numeric(M)
for (k in 1:M) {    # transformation y(x)
  x = r[k];
  if (x < 0.5) {y[k] = sqrt(x/2)}
  else {y[k] = 1-sqrt((1-x)/2)}}
# png('tentDensityEstimate160722.png',width=16,height=16,units='cm',res=300)
plot(density(y,from=0.0,to=1.0),type='l',lwd=4,col='black',
     xlab='y',ylab='Density',main='',las=1,xlim=c(0,1),ylim=c(0,2),cex.lab=1.5)
dy = 0.01; y1 = seq(0,0.5,dy); y2 = seq(0.5,1,dy)
q1 = 4*y1; q2 = 4*(1-y2)
lines(y1,q1,lwd=2,col='magenta',lty=4)
lines(y2,q2,lwd=2,col='magenta',lty=4)
legend('bottom',legend=c('MC estimate','tent PDF'),col=c('black','magenta'),
       lty=c(1,4),lwd=c(4,2),cex=1.5)
# dev.off()