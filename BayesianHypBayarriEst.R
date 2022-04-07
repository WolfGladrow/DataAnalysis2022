print('file: BayesianHypBayarriEst.R')
# Bayarri et al. (2008): formulate as estimation problem: least squares
x = 0:3
y = c(81,9,7,1) # observed frequencies
n = sum(y)
y = y/n # relative frequencies
pa = seq(0.68,0.8,0.001) # trial values for p
Lambdaa = seq(1,1.3,0.001) # trial values for lambda
Lp = length(pa); LL = length(Lambdaa)
M = matrix(data=NA,nrow=Lp,ncol=LL)
library(VGAM)
for(i in 1:Lp) {
  for(j in 1:LL) {
    p = pa[i]; Lambda = Lambdaa[j]
    ypred = dzipois(x,Lambda,p)
    M[i,j] = sum((ypred-y)^2)
  }
}
out = which(M == min(M),arr.ind=TRUE)
popt = pa[out[1]]
Lopt = Lambdaa[out[2]]
Mmin = M[out[1],out[2]]
pPredicted = dzipois(x,Lopt,popt) 
print(c(popt,'optimal p'))
print(c(Lopt,'optimal lambda'))
print(c(Mmin,'minimal sum of squares'))
sflag = 1
if (sflag == 1) {
  # png('PoissonZIPleastSquares191116.png',width=16,height=16,units='cm',res=300)
  contour(pa,Lambdaa,M,xlab='p',ylab=NA,las=1,cex.lab=1.5)
  points(popt,Lopt,col='magenta',lwd=6,cex=0.8)
  title(ylab=TeX('$\\lambda$'),line=2.7,cex.lab=1.5)
  text(0.722,1.05,bquote(~p == .(popt)),col='magenta',pos=4,cex=1.5)
  text(0.731,1.15,bquote(~lambda[opt] == .(Lopt)),col='magenta',pos=4,cex=1.5)
  # dev.off()
}