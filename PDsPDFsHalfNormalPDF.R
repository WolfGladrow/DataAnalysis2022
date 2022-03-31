print('file: PDsPDFsHalfNormalPDF.R')
# half-normal PDF
x0 = seq(-4,4,0.01); y0 = dnorm(x0)
x = seq(0,4,0.01); y = 2*dnorm(x)
# png('HalfNormalPDF200705.png',width=16,height=16,units='cm',res=300)
plot(x0,y0,type='l',xlab='x',ylab='Density',col='blue',lwd=1,
     las=1,lty=2,xlim=c(-3,3),ylim=c(0,1),cex.lab=1.5) 
lines(x,y,col='black',lwd=3)
abline(v=0,col='magenta',lty=4)
legend('topleft',legend=c('half normal','normal'),col=c('black','blue'),
       lty=c(1,2),lwd=c(3,1),cex=1.5)
# dev.off()