print('file: MDintegrateSingularity.R')
# integral with singularity at x=0
integrand3 = function(x) 1/sqrt(x)
I3 = integrate(integrand3,0,1)$value; print(c(I3,'I3, numerical'))
I3ana = 2*sqrt(1)-2*sqrt(0);          print(c(I3ana,'I3analytical'))
x = seq(0.01,1,0.01); y = integrand3(x); yi = 2*sqrt(x)
# png('IntSqrt170220.png',width=16,height=12,units='cm',res=300)
plot(x,y,type='l',lwd=2,col='black',xlab='x',ylab='y',las=1,ylim=c(0,10),
     cex.lab=1.5,lty=4)
lines(x,yi,col='blue',lwd=3,lty=1)
# dev.off()