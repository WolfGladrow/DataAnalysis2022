print('file: MVCCexampleCovariance.R')
print(date())
x = c(5.1,4.8,1.8,4.5,6.6,2.2,5.7,5.7,5.9,9.7)
y = c(7.8,8.2,3.5,6.7,8.9,3.2,8.7,8.5,7.2,14.2)
xmean = mean(x); print(c(round(xmean,2),'xmean'))
ymean = mean(y); print(c(round(ymean,2),'ymean'))
c = cov(x,y); print(c(round(c,2),'covariance'))
r = cor(x,y); print(c(round(r,4),'correlation'))
# png('CovExample220222.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='p',col='blue',lwd=4,cex=0.6,las=1,
     xlim=c(0,10),ylim=c(0,15),xlab='x',ylab='y',cex.lab=1.5)
xp1 = c(xmean,xmean); yp1 = c(-10,20)
xp2 = c(-1,12); yp2 = c(ymean,ymean)
lines(xp1,yp1,col='red'); lines(xp2,yp2,col='red')
text(6,12,'+',col='red',cex=2)
text(4.4,12,'-',col='red',cex=2)
text(6,3,'-',col='red',cex=2)
text(4.4,3,'+',col='red',cex=2)
# dev.off()
# -----------------------------------------------------------------------------
# Results:
# "file: MVCCexampleCovariance.R"
# "Sun Dec 18 07:33:32 2022"
# "5.2"     "xmean"
# "7.69"    "ymean"
# "6.64"    "covariance"
# "0.9739"  "correlation"
# -----------------------------------------------------------------------------
