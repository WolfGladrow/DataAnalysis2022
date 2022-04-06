print('file: LikelihoodFctPoisson.R')
# likelihood function for Poisson PD: n = 1 or 2
k1 = 3; k2 = 2   # integer values
LF1fct = function(lambda,k) {f = lambda^k*exp(-lambda)/factorial(k)}
out1=integrate(LF1fct,lower=0,upper=Inf,k=k1)
LF2fct = function(lambda,ka,kb) {
  f = lambda^(ka+kb)*exp(-2*lambda)/(factorial(ka)*factorial(kb))}
out2=integrate(LF2fct,lower=0,upper=Inf,ka=k1,kb=k2)
lambdaa = seq(0,10,0.01)
f1 = LF1fct(lambdaa,k1)
f2 = LF2fct(lambdaa,k1,k2)
# png('LFPoisson181205.png',width=16,height=16,units='cm',res=300)
plot(lambdaa,f1,type='l',lwd=3,col='blue',xlab=expression(lambda),
     ylab='Likelihood functions',las=1,cex=0.4,cex.lab=1.5)
lines(lambdaa,f2,lwd=2,col='black',lty=2)
legend('topright',legend=c('(3)','(3,2)'),col=c('blue','black'),lty=c(1,2),lwd=c(3,2),cex=1.5)
# dev.off()