print('file: LillieforsCDF-MC.R')
# Lilliefors CDF & p-values: Monte Carlo simulations
M = 1e5  # number of Monte Carlo runs
n = 30   # sample size
Darr = numeric(M)
set.seed(1953) # set seed for random number generators
for(k in 1:M) {x=rnorm(n); z = (x-mean(x))/sd(x);
   Darr[k] = ks.test(z,'pnorm')$statistic}
# outKS = ks.test(z,'pnorm'); Darr[k] = outKS$statistic}
# direct 'integration' (without estimation of density):
sDarr = sort(Darr); dy = 1/M; CDFest = seq(dy,1,dy)
alpha = 0.05
k0 = which.min( (CDFest-(1-alpha))^2 )
Dcrit = sDarr[k0]; Dcritr = round(Dcrit,3)  # 0.1591796
xp = c(Dcrit,Dcrit); yp = c(CDFest[k0],1)
xp2 = c(min(sDarr),Dcrit); yp2 = c(0.95,0.95)
Dapp = seq(0.08,0.2,0.01); Lapp = length(Dapp); papp = numeric(Lapp)
source('LilliePapprox.R')
for(k in 1:Lapp) papp[k] = LilliePapprox(n,Dapp[k])
sflag = 1
if(sflag == 1) {
# png('LillieforsN30Est160916.png',width=16,height=16,units='cm',res=300)
plot(sDarr,CDFest,type='l',lwd=3,col='blue',xlab='D',
         ylab='Estimate of Lilliefors CDF',las=1,cex=0.4,cex.lab=1.5)
lines(xp,yp,col='red',lty=2,lwd=2)
lines(xp2,yp2,col='magenta',lty=1,lwd=1)
text(0.2,0.6,bquote(~D[c] == .(Dcritr)),col='red',cex=1.5)
text(0.2,0.2,bquote(~n == .(n)),col='black',cex=1.5)
text(0.07,0.85,'CDF = 0.95',col='magenta',cex=1.5)
points(Dapp,1-papp,col='black',lty=1,lwd=4,cex=0.6)
# dev.off()
}
D1 = 0.0925906
k1 = which.min( (sDarr-D1)^2) 
p = 1-CDFest[k1]  # 0.72957