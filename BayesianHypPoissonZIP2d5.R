print('file: BayesianHypPoissonZIP2d5.R')
# ZIP vs. Poisson
sflag = 2
if (sflag == 2) {
  p = 0.4; lambda = 2.5
  x = 0:10
  yp = dpois(x,lambda)
  y = (1-p)*yp
  y[1] = y[1] + p
  # png('PoissonZIP2d5r19114.png',width=16,height=12,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',
       ylab='ZIP, Poisson',las=1,cex=0.6,cex.lab=1.5)
  points(x,yp,col='magenta',lwd=4,cex=0.6,pch=24)
  text(5,0.35,bquote(~lambda == .(lambda)),col='black',pos=4,cex=1.5)
  text(5,0.25,bquote(~p == .(p)),col='black',pos=4,cex=1.5)
  # dev.off()
}
print(c(round(y[1],4),'ZIP(x=0)'))
print(c(round(yp[1],4),'Poisson(x=0)'))