print('file: MMkinetics.R')
# Michaelis-Menten kinetics
Vmax = 4; K = 2; S = seq(0,10,0.01); V = Vmax*S/(S+K)
xK = c(K,K); yK = c(0,Vmax/2)
xSK = c(0,K); ySK = c(Vmax/2,Vmax/2)
# png('MichaelisMenten210228a.png',width=16,height=16,units='cm',res=300)
plot(S,V,type='l',lwd=3,col='blue',xlab='Substrate S',ylab='Rate V(S)',
     las=1,cex=0.4,ylim=c(0,5),cex.lab=1.5)
lines(xK,yK,col='black',lty=2)
lines(xSK,ySK,col='black',lty=4)
abline(h=Vmax,col='black',lty=3)
text(7,4.3,TeX('$V_{max} = 4$'),col='blue',cex=1.5)
text(2.1,1.9,'K = 2',col='blue',pos=4,cex=1.5)
# dev.off()