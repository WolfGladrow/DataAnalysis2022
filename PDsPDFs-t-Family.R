print('file: PDsPDFs-t-Family.R')
# family of Student t PDFs with fat tails (Maronna et al., 2006, p.20)
x = seq(-3,3,0.01)
nu = 1   # > 0 degrees of freedom: nu=1: Cauchy; nu -> infinity: normal
cnu = gamma((nu+1)/2)/(sqrt(nu*pi)*gamma(nu/2))  # normalization constant
fnu = cnu*(1+x^2/nu)^(-(nu+1)/2)
nu1 = 5   # > 0 degrees of freedom: nu=1: Cauchy; nu -> infinity: normal
cnu1 = gamma((nu1+1)/2)/(sqrt(nu1*pi)*gamma(nu1/2))  # normalization constant
fnu1 = cnu1*(1+x^2/nu1)^(-(nu1+1)/2)
nu2 = 100   # > 0 degrees of freedom: nu=1: Cauchy; nu -> infinity: normal
cnu2 = gamma((nu2+1)/2)/(sqrt(nu2*pi)*gamma(nu2/2))  # normalization constant
fnun2 = cnu2*(1+x^2/nu2)^(-(nu2+1)/2)
# png('StudentFamily160723.png',width=16,height=16,units='cm',res=300)
plot(x,fnu,type='l',lwd=3,col='blue',xlab='x',ylab='Density',las=1,ylim=c(0,0.4),cex.lab=1.5)
lines(x,fnu1,col='black',lty=2,lwd=3)
lines(x,fnun2,col='magenta',lty=4,lwd=3)
legend('topleft',legend=c('Cauchy','t','almost normal'),col=c('blue','black','magenta'),
       lty=c(1,2,4),lwd=c(3,3,3),cex=1)
text(1,0.33,bquote(~nu == .(nu)),col='blue',pos=4,cex=1.5)
text(1,0.36,bquote(~nu == .(nu1)),col='black',pos=4,cex=1.5)
text(1,0.39,bquote(~nu == .(nu2)),col='magenta',pos=4,cex=1.5)
# dev.off()