print('file: MC-F-PDFs-VRTnormal.R')
# variance ratio test for sampling from normal PDFs (Monte Carlo simulation)
# purpose: Estimate PDF of F with Monte Carlo simulations under the assumption that
#          the samples stem from normal PDFs with different means, however, equal variances
set.seed(1953) # set seed for random number generators
m=11; n=10 #   sample sizes
my.Fsimulation=function(){ # generate random samples from discrete uniform PD
  z1=rnorm(m,mean=1,sd=1.5); z2=rnorm(n,mean=2,sd=1.5); F.stats = var(z1)/var(z2)}
Fstat.vector=replicate(1e6,my.Fsimulation())
out = density(Fstat.vector,from=0,to=3)  # note: specify the range for density estimation
xd = out$x; yd = out$y; yF = df(xd,df1=m-1,df2=n-1); d = yF-yd
sflag = 1
if(sflag == 1) {
  # png('VartestuniformPDnorm170704.png',width=16,height=16,units='cm',res=300)
  plot(xd,yd,type='l',xlim=c(0,3),ylim=c(-0.1,0.8),lwd=4,main='',xlab='F',
       ylab='Density',col='black',cex.lab=1.5)
  lines(xd,yF,col='magenta',lwd=4,lty=4)
  lines(xd,d,col='blue',lty=1)
  legend('topright',c('Monte Carlo','F(x;10,9)','difference'),lwd=c(3,3,1),
         col=c('black','magenta','blue'),lty=c(1,4,1))
  # dev.off()
}