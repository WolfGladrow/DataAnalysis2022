print('file: BayesianHypBayarriEstFit.R')
print(date())
# Bayarri et al. (2008): formulate as estimation problem: least squares
# fit zero-inflated Poisson distribution
x = 0:3
y = c(81,9,7,1) # observed frequencies
n = sum(y)
y = y/n # relative frequencies
pa = seq(0.68,0.8,0.001) # trial values for p
Lambdaa = seq(1,1.3,0.001) # trial values for lambda
Lp = length(pa); LL = length(Lambdaa)
M = matrix(data=NA,nrow=Lp,ncol=LL)
# install.packages('VGAM')
library(VGAM) # contains dzipois() = zero-inflated Poisson distribution
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
  # png('PoissonZIPleastSquaresRF191116.png',width=16,height=16,units='cm',res=300)
  plot(x,y,type='p',lwd=4,col='blue',xlab='x',
       ylab='Probabilities, relative frequencies',las=1,cex=0.6,cex.lab=1.5)
  points(x,pPredicted,col='magenta',lwd=4,cex=0.6,pch=24)
  text(1,0.6,bquote(~p == .(popt)),col='magenta',pos=4,cex=1.5)
  text(1,0.7,bquote(~lambda[opt] == .(Lopt)),col='magenta',pos=4,cex=1.5)
  # dev.off()
# ---------------------------------------------------------------------------
# Results:
# "file: BayesianHypBayarriEstFit.R"
# "Sat Dec 17 09:09:13 2022"
# "0.745"     "optimal p"
# "1.183"          "optimal lambda"
# "0.000421849150042302"   "minimal sum of squares"
# ---------------------------------------------------------------------------
  