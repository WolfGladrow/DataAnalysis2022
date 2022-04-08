print('file:KSunifNormal.R')
# KS-test: uniform versus normal
print(' ---------------------------------------------------')
print('56 = KS-test: uniform versus normal (4/2022)')
print(' ---------------------------------------------------')
set.seed(1953)
n = 30
x = rnorm(n) # sample from standard normal PDF
qN = ks.test(x,'pnorm') 
pN = qN$p.value; print(c(round(pN,6),'KS-test: p standard normal PDF'))
DN = as.numeric(qN$statistic); print(c(round(DN,4),'KS-test: D standard mormal PDF'))
beta = sqrt(3) # +-beta are limits of standardized uniform PDF, i.e. uniform PDF with
#        mean mu = 0 and standard deviation sigma = 1
q = ks.test(x,'punif',min=-beta,max=beta)
p6 = q$p.value; print(c(round(p6,6),'KS-test: p standardized uniform PDF'))
D6 = as.numeric(q$statistic); print(c(round(D6,4),'KS-test: D standardized uniform PDF'))
xECDF = ecdf(sort(x))
 # CDF of standardized uniform & normal CDF with same mu & sigma
xp = seq(-3,3,0.01)
yU = punif(xp,-beta,beta) # CDF
yN = pnorm(xp)            # CDF
dabs = abs(yU-yN)
k = which.max(dabs)
print(c(round(dabs[k],4),'maximal absolute difference between U & N CDF'))
# png('CDFunifNorm200713.png',width=16,height=16,units='cm',res=300)
plot(xp,yU,type='l',lwd=3,col='blue',xlab='x',ylab='CDF',las=1,cex=0.4,cex.lab=1.5)
lines(xp,yN,col='black',lwd=3,lty=2)
lines(ecdf(sort(x)),col='magenta')
legend('topleft',legend=c('standardized uniform','standard normal'),col=c('blue','black'),
         lty=c(1,2),lwd=c(3,3),cex=1.2)
text(0.4,0.22,paste('D = ',as.character(round(D6,2))),col='blue',pos=4,cex=1.5)
text(0.4,0.1,paste('p = ',as.character(round(p6,2))),col='blue',pos=4,cex=1.5)
text(-2,0.72,paste('D = ',as.character(round(DN,2))),col='black',pos=4,cex=1.5)
text(-2,0.6,paste('p = ',as.character(round(pN,2))),col='black',pos=4,cex=1.5)
# dev.off()