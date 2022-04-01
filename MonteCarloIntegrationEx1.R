print('file: MonteCarloIntegrationEx1.R')
# classical Monte Carlo integration: example I
h = function(y) {cos(y)}
hf = function(y) {h(y)*dnorm(y)}
Ianalytic = exp(-1/2)   # 0.6065307
out1 = integrate(hf,lower=-Inf,upper=Inf)
Inumeric = out1$value   # 0.6065307
set.seed(1953) # set seed for random number generators
n = 2000; x = rnorm(n)  # random sample from standard normal PDF
estEh = numeric(n)
for(k in 1:n) estEh[k] = mean(h(x[1:k]))
estEh[n]  # 0.6024225
# png('MCclassicalExampleI160812.png',width=16,height=16,units='cm',res=300)
na = seq(1,n)
plot(na,estEh,type='p',lwd=1,col='blue',xlab='n',ylab='Estimate of E[h]',las=1,
     cex=0.2,cex.lab=1.5)
xp=c(1,n); yp = c(Ianalytic,Ianalytic)
lines(xp,yp,col='black',lty=2)
# dev.off()