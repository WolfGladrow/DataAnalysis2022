print('file: MonteCarloIntegrationEx3.R')
# Monte Carlo integration: example from Robert & Casella (2009, p.66)
set.seed(1953) # set seed for random number generators
h=function(x){(cos(50*x)+sin(20*x))^2}
a = 20; b = 50
# png('Robert09CasellaFig3d3m160811.png',width=16,height=16,units='cm',res=300)
par(mar=c(4.5,4.5,4.5,1),mfrow=c(2,1))
curve(h,ylab='Function',xlab='x',lwd=2,las=1,col='blue',cex.lab=1.5)
integrate(h,0,1) # 0.9652009 with absolute error < 1.9e-10
Ianalytic = 1-sin(2*a)/(4*a)+sin(2*b)/(4*b)-
  cos(a-b)/(a-b)-cos(a+b)/(a+b)+1/(a-b)+1/(a+b) # 0.9652009
x=h(runif(10^4))
estint=cumsum(x)/(1:10^4)
esterr=sqrt(cumsum((x-estint)^2))/(1:10^4)
plot(estint,ylab='Estimate',type='l',las=1,lwd=2,
     ylim=mean(x)+20*c(-esterr[10^4],esterr[10^4]),xlab='n',cex.lab=1.5)
lines(estint+2*esterr,col='gold',lwd=2)
lines(estint-2*esterr,col='gold',lwd=2)
# dev.off()