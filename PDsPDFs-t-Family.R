print('file: PDsPDFs-t-Family.R')
# family of Student t PDFs with fat tails (Maronna et al., 2006, p.20)
x = seq(-3,3,0.01)
nu = 1   # > 0 degrees of freedom: nu=1: Cauchy; nu -> infinity: normal
cnu = gamma((nu+1)/2)/(sqrt(nu*pi)*gamma(nu/2))  # normalization constant
fnu = cnu*(1+x^2/nu)^(-(nu+1)/2)
nu1 = 5   # > 0 degrees of freedom: nu=1: Cauchy; nu -> infinity: normal
cnu1 = gamma((nu1+1)/2)/(sqrt(nu1*pi)*gamma(nu1/2))  # normalization constant
fnu1 = cnu1*(1+x^2/nu1)^(-(nu1+1)/2)
nu = 100   # > 0 degrees of freedom: nu=1: Cauchy; nu -> infinity: normal
cnu = gamma((nu+1)/2)/(sqrt(nu*pi)*gamma(nu/2))  # normalization constant
fnun = cnu*(1+x^2/nu)^(-(nu+1)/2)
# png('StudentFamily160723.png',width=16,height=12,units='cm',res=300)
plot(x,fnu,type='l',lwd=3,col='blue',xlab='x',ylab='PDFs',las=1,ylim=c(0,0.4),cex.lab=1.5)
lines(x,fnu1,col='black',lty=2,lwd=3)
lines(x,fnun,col='red',lty=3,lwd=3)
legend('topleft',legend=c('Cauchy','t','almost normal'),col=c('blue','black','red'),
       lty=c(1,2,3),lwd=c(3,3,3))
# dev.off()