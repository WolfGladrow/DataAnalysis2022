print('file: MonteCarloIntegrationEx2.R')
# classical Monte Carlo integration: example 2
h = function(y) {cos(y)}  # int(cos) = sin
set.seed(1953) # set seed for random number generators
a=0; b=2;
Ianalytic = sin(b)-sin(a)  # 0.9092974
n = 10000; x = runif(n,min=a,max=b)  # random sample from uniform PDF
estEh = numeric(n)
for(k in 1:n) estEh[k] = (b-a)*mean(h(x[1:k]))
library(latex2exp)
# png('MCclassicalExampleII160812.png',width=16,height=16,units='cm',res=300)
na = seq(1,n)
plot(na,estEh,type='p',lwd=1,col='blue',xlab='n',
     ylab='Estimate',las=1,cex=0.1,cex.lab=1.5)
xp = c(1,n); yp = c(Ianalytic,Ianalytic)
text(1000,1.1,TeX('$I_2 = \\int_0^2 \\cos\\, x \\, dx = \\sin\\, 2 \\approx 0.909$'),
     col='black',pos=4,cex=1.5)
lines(xp,yp,col='black',lty=2)
# dev.off()