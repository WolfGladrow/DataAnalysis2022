print('file: MM-Lineweaver-Burk-Trans.R')
# illustrate Lineweaver-Burk transformation
Vmax = 4; K = 2; S = seq(0.25,100,0.05); V = Vmax*S/(S+K)
# 1/V = (K/Vmax)/S + 1/Vmax
Si = 1/S; Vi = 1/V
xp1 = c(2,3); yp1 = c(1,1)*2*K/Vmax+1/Vmax
xp2 = c(3,3); yp2 = c(2*K/Vmax+1/Vmax,3*K/Vmax+1/Vmax)
sflag = 1
if (sflag == 1) {
# png('LineweaverIllustration210228.png',width=16,height=12,units='cm',res=300)
plot(Si,Vi,type='l',lwd=3,col='black',xlab='1/S',ylab='1/V',las=1,cex.lab=1.5,
     xlim=c(0,4),ylim=c(0,2.5),xaxs='i',yaxs='i')
points(0,1/Vmax,col='blue',lwd=5,cex=1.2)
text(0.1,1/Vmax*0.8,TeX('$1/V_{max} = 0.25$'),col='blue',pos=4,cex=1.5)
lines(xp1,yp1,col='blue',lty=4)
lines(xp2,yp2,col='blue',lty=4)
text(1.95,1.1,TeX('$slope = K/V_{max} = 0.5$'),col='blue',pos=4,cex=1.5)
text(0.5,2.3,TeX('$V_{max} = 4$'),col='black',pos=4,cex=1.5)
text(0.5,2.0,TeX('K = 2$'),col='black',pos=4,cex=1.5)
# dev.off()
}