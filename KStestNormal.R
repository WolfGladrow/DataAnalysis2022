print('file: KStestNormal.R')
# Kolmogorov-Smirnov (KS) test for random sample from normal PDF
set.seed(1953) # set seed for random number generators
n = 30 # sample size (should be >= 30 for application of KS test)
x = rnorm(n)
xs = (x-mean(x))/sd(x) # standardized random sample
narr = seq(1,n)
xH0 = seq(-3,3,0.01); yH0 = pnorm(xH0) # CDF for standard normal PDF
source('plotstaircaseKS.R')
out1KS = ks.test(x,'pnorm')
D = as.numeric(out1KS$statistic); print(c(round(D,4),'D for x'))
p = as.numeric(out1KS$p.value); print(c(round(p,4),'p for x'))
out1KSs = ks.test(xs,'pnorm')
Ds = as.numeric(out1KSs$statistic); print(c(round(Ds,4),'D for xs'))
ps = as.numeric(out1KSs$p.value); print(c(round(ps,4),'p for xs'))
out1KSc = ks.test(x,'pnorm',mean(x),sd(x))
D1 = as.numeric(out1KSc$statistic); print(c(round(D1,4),'D for x, ks.test(x,m,s)'))
p1 = as.numeric(out1KSc$p.value); print(c(round(p1,4),'p for x, ks.test(x,m,s)'))
sflag = 1
if (sflag == 1) {
  # png('KSstdnormal200713.png',width=16,height=16,units='cm',res=300)
  plotstaircaseKS(sort(x),-3,3,xH0,yH0)
  text(1,0.4,paste('D = ',as.character(round(D,4))),col='blue',cex=1.5,pos=4)
  text(1,0.2,paste('p = ',as.character(round(p,4))),col='blue',cex=1.5,pos=4)
  legend('topleft',legend=c('estimate','normal CDF'),col=c('blue','black'),
         lty=c(1,1),lwd=c(3,3),cex=1.5)
  # dev.off()
}
if (sflag == 3) {
  # png('KSstdnormalECDF220313.png',width=16,height=16,units='cm',res=300)
  plot(ecdf(sort(x)),main='',col='blue',cex=1.5,ylab='CDF',cex.lab=1.5)
  lines(xH0,yH0,col='black',lwd=3)
  text(1,0.4,paste('D = ',as.character(round(D,4))),col='blue',cex=1.5,pos=4)
  text(1,0.2,paste('p = ',as.character(round(p,4))),col='blue',cex=1.5,pos=4)
  legend('topleft',legend=c('estimate','normal CDF'),col=c('blue','black'),
         lty=c(1,1),lwd=c(3,3),cex=1.5)
  # dev.off()
}
# install.packages('kolmim')
library(kolmim)
pK = (1-pkolm(D,n))
pM = (1-pkolmim(D,n))
out1KSimp = ks.test.imp(x,'pnorm')
print(c(round(pK,4),' pK'))
print(c(round(pM,4),' pM'))