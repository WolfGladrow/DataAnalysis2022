print('file: InvChisqPDF.R')
# scaled inverse-chisq PDF
# install.packages('LaplacesDemon') 
library(LaplacesDemon) # scaled inverse-chi^2 & inverse-gamma PDF
print(' ---------------------------------------------------')
print('Inverse gamma PDF:')
dx = 0.01; x = seq(dx,2,dx)
phi = 2.3; psi = 0.5
nu = 2*phi; tausq = psi/phi; tausqr = round(tausq,4)
print(c(nu,'nu')); print(c(round(tausq,4),'tausq'))
y5 = dinvchisq(x,nu,tausq)
library(latex2exp)
# png('ScaledInverseChisq210618.png',width=16,height=16,units='cm',res=300)
plot(x,y5,type='l',lwd=3,col='black',xlab='x',ylab='Density',las=1,cex=0.4,cex.lab=1.5)
xt = 1
text(xt,2.5,bquote(~nu == .(nu)),col='black',pos=4,cex=1.5)
text(xt,2,bquote(~tau^2 == .(tausqr)),col='black',pos=4,cex=1.5)
# dev.off()