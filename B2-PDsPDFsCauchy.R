print('file: B2-PDsPDFsCauchy.R')
# Cauchy distribution
x = seq(-4,4,0.01); Cauchy = dcauchy(x)
Normal = dnorm(x)
# png('CauchyPDF220228.png',width=16,height=16,units='cm',res=300)
plot(x,Cauchy,type='l',col='black',xlab='x',ylab='Density',lwd=4,
     las=1,xlim=c(-3,3),ylim=c(0,max(Normal)),cex.lab=1.5)
lines(x,Normal,col='blue',lty=2)
legend('topleft',legend=c('Cauchy','normal'),col=c('black','blue'),
       lty=c(1,2),lwd=c(4,1),cex=1.5)
# dev.off()