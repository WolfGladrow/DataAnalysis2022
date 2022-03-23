print('file: MDintegrateInfRange.R')
# numerical integration over an infinite range
integrand2 = function(x) exp(-x)
I2 = integrate(integrand2,0,Inf)$value; print(c(I2,'I2, numerical'))
I2ana = -exp(-Inf)+exp(0);              print(c(I2ana,'I2analytical'))
x = seq(0,10,0.1); y = integrand2(x); 
yi = 1-exp(-x) # integral over exp(-x) from 0 to x
# png('IntExp170220.png',width=16,height=12,units='cm',res=300)
plot(x,y,type='l',lwd=3,col='black',xlab='x',ylab='y',las=1,cex.lab=1.5,lty=4)
lines(x,yi,col='blue',lwd=2,lty=1)
# dev.off()
