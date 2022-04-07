print('file: MC-F-PDFs-VRTcount.R')
# variance ratio test for sampling from uniform PDs (Monte Carlo simulation)
# use centered distributions
# purpose: Estimate PDF of F with Monte Carlo simulations under the assumption that
#          the samples stem from uniform probability distributions (integers, discrete!)
#          with equal variances

# Zar (2010, Example 8.7): 
x1 = c(41,35,33,36,40,46,31,37,34,30,38) # number of moths in trap #1
x2 = c(52,57,62,55,64,57,56,55,60,59)    # number of moths in trap #2
x1min = min(x1); x1max = max(x1)
x2min = min(x2); x2max = max(x2)
x1x2range = max(x1max-x1min,x2max-x2min)
# ----------------------------------------------------------------------
set.seed(1953) # set seed for random number generators
m=11; n=10 #   sample sizes}
my.Fsimulation=function(){ # generate random samples from discrete uniform PD
  xrange1 = seq(x1min,x1min+x1x2range); xrange2 = seq(x2min,x2min+x1x2range); # equal var.
  z1=sample(x=xrange1,m,replace=TRUE); z2=sample(x=xrange2,n,replace=TRUE);
  F.stats = var(z1)/var(z2)}
Fstat.vector=replicate(1e6,my.Fsimulation())
out = density(Fstat.vector,from=0,to=3)
xd = out$x; yd = out$y; yF = df(xd,df1=m-1,df2=n-1); d = yF-yd
Fobs = 0.60129  # observed F value (example)
k = which.min((xd-Fobs)^2)  # find xd = Fobs
# xd[k]  # 0.5988258
dx = xd[2]-xd[1]  # equidistant
pMC = dx*sum(yd[1:k])  
pMC2 = round(2*pMC,2); print(c(pMC2,'pMC2'))
pLeft = pf(Fobs,df1=m-1,df2=n-1); print(c(round(pLeft,4),'pleft'))
p = round(2*pLeft,2); print(c(p,'p'))
sflag = 2
if (sflag == 2) {
  # png('VarTestuni50PDab171225MC.png',width=16,height=16,units='cm',res=300) # x2min=50
  plot(xd,yd,type='l',xlim=c(0,3),ylim=c(-0.4,1),lwd=4,main='',xlab='F',
       ylab='Density',col='black',cex.lab=1.5)
  lines(xd,yF,col='magenta',lwd=4,lty=4)
  lines(xd,d,col='blue',lwd=2,lty=1)
  abline(v=Fobs,col='green',lty=3)
  legend('topright',c('Monte Carlo','F(x;10,9)','difference'),lwd=c(3,3,1),
         col=c('black','magenta','blue'),lty=c(1,4,1),cex=1.5)
  xt=2
  text(xt,-0.1,bquote(~p[MC] == .(pMC2)),col='black',pos=4,cex=1.5)
  text(xt,-0.3,bquote(~p == .(p)),col='magenta',pos=4,cex=1.5)
  # dev.off()
}
# ----------------------------------------------------------------
# Remarks:
# my.Fsimulation=function(){ ... } defines a function without input
# replicate(1e6,my.Fsimulation())  applies this function 1 million times
# ----------------------------------------------------------------
