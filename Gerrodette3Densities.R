print('file: Gerrodette3Densities.R')
# Gerrodette11: plot neg. binomial, normal, lognormal
LNparams = function(mu,sigma){
  # Calculate parameters alpha, beta for lognormal PDF from
  # mean mu > 0 and standard deviation sigma
  # Dieter.Wolf-Gladrow@awi.de 7/2019 (based on function lnormal.params from bayescount)
  beta = sqrt(log(sigma^2/mu^2+1))
  alpha = log(mu) - ((beta^2)/2)
  return(c(alpha,beta))
}
mu = 409; sigma = 250 
print(c(mu,'mean'))
print(c(sigma,'standard deviation'))
x = seq(1,1000)
par = LNparams(mu,sigma)
yLN = dlnorm(x,par[1],par[2])
print(c(round(par[1],4),'alpha (lognormal PDF)'))
print(c(round(par[2],4),'beta (lognormal PDF)'))
yNormal = dnorm(x,mu,sigma)
# Calculate parameters s and p for negative binomial PD:
s = mu^2/(sigma^2-mu); p = s/(mu+s)
print(c(round(s,4),'s (negative binomial PD)'))
print(c(round(p,5),'p (negative binomial PD)'))
xNB = seq(1,1000,10)
yNB = dnbinom(xNB,s,p)
sflag = 2
if (sflag == 2) {
  sc = 1e3 # scale for plot
  # png('Gerrodette11NegBiLogNorm200724.png',width=16,height=12,units='cm',res=300)
  plot(x,yLN*sc,type='l',lwd=3,col='blue',xlab='Abundance',
       ylab='Densities*1000',las=1,cex=0.4,cex.lab=1.5)
  lines(x,yNormal*sc,col='black',lty=2,lwd=3)
  points(xNB,yNB*sc,col='magenta',lwd=3,cex=0.4)
  lines(x,yLN*sc,col='blue',lwd=3)
  text(400,2.3,col='blue','lognormal PDF',pos=4,cex=1.5)
  text(700,1.4,col='black','normal PDF',pos=4,cex=1.5)
  text(450,1.85,col='magenta','negative binomial PD',pos=4,cex=1.5)
  # dev.off()
}