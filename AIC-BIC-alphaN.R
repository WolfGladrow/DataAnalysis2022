print('file: AIC-BIC-alphaN.R')
# Dziak et al. (2018, Table 2, p.12) AIC & BIC
AnAIC = 2
alphaAIC = 1-pchisq(AnAIC,1)
n = seq(3,100)
L = length(n)
alphaAICa = rep(alphaAIC,L)
alphaBIC = 1-pchisq(log(n),1)
# ------------
# png('Dziak18alphaSCRIPT181121.png',width=16,height=12,units='cm',res=300)
plot(n,alphaBIC,type='p',lwd=3,col='blue',xlab='n',
     ylab=expression(alpha),las=1,cex=0.4,ylim=c(0,0.21),cex.lab=1.5)
points(n,alphaAICa,col='black',pch=23,cex=0.4,lwd=3)
text(50,0.09,'BIC',col='blue',pos=1,cex=1.5); text(50,0.20,'AIC',col='black',pos=1,cex=1.5)
# dev.off()