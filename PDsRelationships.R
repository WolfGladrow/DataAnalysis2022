print('file: PDsRelationships.R')
# Leemis (1986) diagram: PDs, modified')
# Bernoulli, binomial, hypergeometric, Poisson, uniform, ...
# box:
phi1 = seq(45,135); sr2=sqrt(2)
xphi1 = sr2*sin(pi*phi1/180)
yphi1 = sr2*cos(pi*phi1/180)
xB = c(-1,1,xphi1,1,-1,-xphi1)
yB = c(1,1,yphi1,-1,-1,-yphi1)
# longer box:
xBL = c(-1,1,xphi1,1,-1,-xphi1)*1.5
yBL = c(1,1,yphi1,-1,-1,-yphi1)
sc = 7 # scale
library(latex2exp)
# png('PDsRelations220625.png',width=16,height=16,units='cm',res=300)
 plot(NA,type='p',lwd=4,col='blue',xlab='',ylab='',xlim=c(0,100),ylim=c(0,100),
       col.axis='white',xaxt='n',yaxt='n',bty='n')
# ----------------------------------- binomial:
  x0=45; y0=75
  lines(xB*sc+x0,yB*sc+y0,col='black')
  text(x0,y0+8,'binomial',col='blue',cex=0.7,pos=1)
  text(x0,y0+4,'x=0,1,...,n',col='black',cex=0.7,pos=1)
  text(x0,y0+0,'n,p',col='black',cex=0.7,pos=1)
  arrows(min(xB*sc+x0)-1,y0,21,70,lty=1,col='black',angle=10,length=0.1)
  arrows(69,60,max(xB*sc+x0)+1,y0-1,lty=4,col='magenta',angle=30,length=0.15)
  text(60,70,'n trials',col='magenta',cex=0.7,pos=4)
  arrows(64,90,max(xB*sc+x0)+1,y0+1,lty=1,col='black',angle=10,length=0.1)
  # ----------------------------------- negative binomial:
  x0=85; y0=40
  lines(xBL*sc+x0,yBL*sc+y0,col='black')
  text(x0,y0+8,'neg. binomial',col='blue',cex=0.7,pos=1)
  text(x0,y0+4,'k=0,1,...,n',col='black',cex=0.7,pos=1)
  text(x0,y0+0,'s,p',col='black',cex=0.7,pos=1)
  # ----------------------------------- hypergeometric
  x0 = 80; y0 = 90 # location of box center
  lines(xBL*7+x0,yBL*7+y0,col='black')
  text(x0,y0+8,'hypergeometric',col='blue',cex=0.7,pos=1)
  text(x0,y0+4,'k=0,1,...,W',col='black',cex=0.7,pos=1)
  text(x0,y0,'N,W,J',col='black',cex=0.7,pos=1)
  # ----------------------------------- Bernoulli
  x0=80; y0=60
  lines(xB*sc+x0,yB*sc+y0,col='black')
  text(x0,y0+8,'Bernoulli',col='blue',cex=0.7,pos=1)
  text(x0,y0+4,'k=0,1',col='black',cex=0.7,pos=1)
  text(x0,y0+0,'p',col='black',cex=0.7,pos=1)
  # ----------------------------------- Poisson
  x0=10; y0=70
  lines(xB*sc+x0,yB*sc+y0,col='black')
  text(x0,y0+8,'Poisson',col='blue',cex=0.7,pos=1)
  text(x0,y0+4,'k=0,1,2, ...',col='black',cex=0.7,pos=1)
  text(x0,y0+0,TeX('$\\lambda$'),col='black',cex=0.7,pos=1)
  arrows(10,61,10,18,lty=4,col='magenta',angle=30,length=0.15)
  arrows(12,61,27,18,lty=4,col='magenta',angle=30,length=0.15)
  # ----------------------------------- Zero Inflated Poisson (ZIP)
  x0=10; y0=10
  lines(xB*sc+x0,yB*sc+y0,col='black')
  text(x0,y0+8,'ZIP',col='blue',cex=0.7,pos=1)
  text(x0,y0+4,'k=0,1,2, ...',col='black',cex=0.7,pos=1)
  text(x0,y0+0,TeX('$\\lambda, p$'),col='black',cex=0.7,pos=1)
  # ----------------------------------- Zero Truncated Poisson (ZTP)
  x0=35; y0=10
  lines(xB*sc+x0,yB*sc+y0,col='black')
  text(x0,y0+8,'ZTP',col='blue',cex=0.7,pos=1)
  text(x0,y0+4,'k=1,2, ...',col='black',cex=0.7,pos=1)
  text(x0,y0+0,TeX('$\\lambda$'),col='black',cex=0.7,pos=1)
  # ----------------------------------- uniform:
  x0 = 10; y0 = 90 # location of box center
  lines(xB*sc+x0,yB*sc+y0,col='black')
  text(x0,y0+8,'uniform',col='blue',cex=0.7,pos=1)
  text(x0,y0+4,'k=1,...,n',col='black',cex=0.7,pos=1)
  text(x0,y0,'n, (p=1/n)',col='black',cex=0.7,pos=1)
  # ----------------------------------- broken stick:
  x0 = 80; y0 = 10 # location of box center
  lines(xB*sc+x0,yB*sc+y0,col='black')
  text(x0,y0+8,'broken stick',col='blue',cex=0.7,pos=1)
  text(x0,y0+4,'k=1,...,n',col='black',cex=0.7,pos=1)
  text(x0,y0,'n',col='black',cex=0.7,pos=1)
  # ----------------------------------- Normal: envelop
  x0=45; y0=40
  lines(xBL*sc+x0,yBL*sc+y0,col='black')
  text(x0,y0+8,'Normal',col='red',cex=0.7,pos=1)
  text(x0,y0+4,TeX('$-\\infty < x < +\\infty$'),col='black',cex=0.7,pos=1)
  text(x0,y0+0,TeX('$\\mu, \\sigma$'),col='black',cex=0.7,pos=1)
  arrows(45,64,45,50,col='red',lty=2,angle=30,length=0.1)
  arrows(20,64,42,50,col='red',lty=2,angle=30,length=0.1)
  arrows(69,40,61,40,col='red',lty=2,angle=30,length=0.1)
# dev.off()