print('file: Rouder09Fig2A.R')
# Rouder09 sigma known: Fig.2A
# measure mu and sample mean in units of sigma:
xmA = 0; xmB = 0.2; xmC = 0.35
n = 100
library(latex2exp)
mu1 = seq(-0.2,1,0.01)
B01A = 1/exp(-mu1^2*n/2)
mu1B1 = seq(-0.2,xmB,0.01)
mu1B2 = seq(xmB,1,0.01)
B01B1 = 1/exp(-mu1B1^2*n/2+n*xmB*mu1B1)
B01B2 = 1/exp(-mu1B2^2*n/2+n*xmB*mu1B2)
mu1C1 = seq(-0.2,xmC,0.01)
mu1C2 = seq(xmC,1,0.01)
B01C1 = 1/exp(-mu1C1^2*n/2+n*xmC*mu1C1)
B01C2 = 1/exp(-mu1C2^2*n/2+n*xmC*mu1C2)
pflag = 1
if (pflag == 1) {
  # png('Rouder09Fig2A191123.png',width=16,height=16,units='cm',res=300)
  plot(mu1,log10(B01A),type='l',lwd=3,col='blue',xlab=TeX('$\\mu_1/\\sigma$'),
       ylab=NA,las=1,cex=0.4,ylim=c(-4,6),cex.lab=1.5)
  title(ylab=TeX('$log_{10}\\, B_{01}$'),line=2.3,cex.lab=1.5)
  abline(h=0,col='red',lty=4)
  lines(mu1B1,log10(B01B1),col='black',lwd=3,lty=1)
  lines(mu1B2,log10(B01B2),col='black',lwd=3,lty=1)
  lines(mu1C1,log10(B01C1),col='magenta',lwd=3,lty=1)
  lines(mu1C2,log10(B01C2),col='magenta',lwd=3,lty=1)
  text(-0.03,2,TeX('$\\bar{x}/\\sigma = 0$'),col='blue',pos=4,cex=1.5)
  text(0.22,0.5,'0.2',col='black',pos=4,cex=1.5)
  text(0.25,-2,'0.35',col='magenta',pos=4,cex=1.5)
  xt = -0.2  # -0.26
  text(xt,5.5,TeX('evidence against $H_{1}$'),col='red',pos=4,cex=1.5)
  text(xt,4.5,TeX('evidence for $H_{0}$'),col='red',pos=4,cex=1.5)
  text(xt,-3.9,TeX('evidence against $H_{0}$'),col='red',pos=4,cex=1.5)
  text(0.45,-3.9,TeX('evidence for $H_{1}$'),col='red',pos=4,cex=1.5)
  # dev.off()
}
print(c(round(min(B01B1),4),'min(B01,xm=0.2)'))
print(c(round(min(B01C1),4),'min(B01,xm=0.35)'))
print(c(round(1/min(B01B1),4),'1/min(B01,xm=0.2)'))
print(c(round(1/min(B01C1),4),'1/min(B01,xm=0.35)'))