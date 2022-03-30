print('file: NeutrinosBayesianFlat.R')
# neutrinos, Bayesian approach: flat prior -> gamma posterior
# (0) data:
k=seq(0,9)  # number of events (neutrinos in 10 s intervals)
frequencies=c(1042,860,307,78,15,3,0,0,0,1) # frequencies
# (1) calculate n and s from data:
n = sum(frequencies)
s = sum(k*frequencies)
# (2) gamma PDF
alpha = s+1; print(c(alpha,'alpha'))
beta = n; print(c(beta,'beta'))
dlambda = 0.001; lambda = seq(0.7,0.9,dlambda)
y = dgamma(lambda,alpha,beta)
muEst = alpha/beta; print(c(round(muEst,3),'muEst'))
varEst = alpha/beta^2; print(c(round(varEst,5),'varEst'))
sdEst = sqrt(alpha/beta^2); print(c(round(sdEst,3),'sdEst'))
muEstr = round(muEst,3)
sdEstr = round(sdEst,3)
xp = c(muEst-sdEst,muEst+sdEst); yp = c(5,5)
# 95% credibility set/credibility interval (CRI):
p = 0.05
xL = qgamma(p/2,alpha,beta);  print(c(round(xL,3),'xL'))
xU = qgamma(1-p/2,alpha,beta);  print(c(round(xU,3),'xU'))
xp95 = c(xL,xU); yp95 = c(0,0)
# png('NeutrinosBayesian220330.png',width=16,height=16,units='cm',res=300)
plot(lambda,y,type='l',lwd=4,col='black',
     xlab=TeX('$\\lambda$'),ylab='Density',las=1,cex=0.6,cex.lab=1.5)
abline(v=muEst,col='black',lty=4)
lines(xp,yp,col='black',lty=2)
lines(xp95,yp95,col='magenta',lwd=3,lty=1)
text(0.8,20,bquote(~hat(lambda) == .(muEstr) %+-% .(sdEstr)),col='black',pos=4,cex=1.5)
text(muEst,1,'95% CRI',col='magenta',cex=1.5)
# dev.off()