print('file: NonStandardized-t-PDF.R')
# non-standardized t-PDF
nu=5; mu = 8; betasq = 0.3; beta = sqrt(betasq)
dx=0.001; x = seq(6,10,dx)
print(' ---------------------------------------------------')
# compare with dt2() from wiqid:
library(wiqid); px2 = dt2(x,mu,sqrt(betasq),nu)
print(' ---------------------------------------------------')
px1 = gamma((nu+1)/2)/(gamma(nu/2)*sqrt(pi*nu*betasq))*
  (1+(x-mu)^2/betasq/nu)^(-(nu+1)/2) # pedestrian
# h = 1/betasq
# px3 = gamma((nu+1)/2)/gamma(1/2)/gamma(nu/2)*sqrt(h/nu)*
#   (1+h/nu*(x-mu)^2)^(-(nu+1)/2) # pedestrian with h-parameterization (not plotted)
mydnst = function(x, location, scale, df) {
  # density of non-standardized t PDF; DWG 6/2021
  tstat = (x-location)/scale
  return(dt(tstat,df)/scale)
  # factor 1/scale in density stems from change of variables: d tstat/dx = 1/scale 
}
px4 = mydnst(x,mu,beta,nu) 
# png('NonStandardized-t-PDF210520.png',width=16,height=16,units='cm',res=300)
plot(x,px4,type='l',lwd=3,col='blue',xlab='x',ylab='Density',las=1,cex=0.4,
     ylim=c(0,max(px1)),cex.lab=1.5) 
lines(x,px1,col='red',lwd=3,lty=2)
lines(x,px2,col='black',lwd=3,lty=3)
text(6.1,0.6,bquote(~nu == .(nu)),col='black',pos=4,cex=1.5)
text(6.1,0.5,bquote(~mu == .(mu)),col='black',pos=4,cex=1.5)
text(6.1,0.4,bquote(~beta^2 == .(betasq)),col='black',pos=4,cex=1.5)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# All three ways to calculate the density lead to identical results. If you don't want
#   to rely on a library, my little function is may be the easiest way to calculate the
#   non-standardized t-PDF.
# ----------------------------------------------------------------