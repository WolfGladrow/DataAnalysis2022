print('file: MVCCexampleCovAnscombe.R')
# plot Anscombe quartet & sample means
# Anscombe's quartet (1973):
data1 = c(10.0,8.04,10.0,9.14,10.0,7.46,8.0,6.58,
          8.0,  6.95,  8.0, 	8.14,  8.0,  6.77, 	8.0, 	5.76,
          13.0,  7.58, 13.0, 	8.74, 13.0, 12.74, 	8.0, 	7.71,
          9.0,  8.81, 	9.0, 	8.77,  9.0,  7.11, 	8.0, 	8.84,
          11.0,  8.33, 11.0, 	9.26, 11.0,  7.81, 	8.0, 	8.47,
          14.0,  9.96, 14.0, 	8.10, 14.0,  8.84, 	8.0, 	7.04,
          6.0,  7.24, 	6.0, 	6.13,  6.0,  6.08, 	8.0, 	5.25,
          4.0,  4.26, 	4.0, 	3.10,  4.0,  5.39, 19.0, 12.50,
          12.0, 10.84, 12.0, 	9.13, 12.0,  8.15, 	8.0, 	5.56,
          7.0,  4.82, 	7.0, 	7.26,  7.0,  6.42, 	8.0, 	7.91,
          5.0,  5.68, 	5.0, 	4.74,  5.0,  5.73, 	8.0, 	6.89)
M1 = matrix(data=data1,nrow=8,ncol=11)
x1=M1[1,]; y1=M1[2,]; x2=M1[3,]; y2=M1[4,]
x3=M1[5,]; y3=M1[6,]; x4=M1[7,]; y4=M1[8,]
# -----------------------------------------
xp0 = c(1,21) 
library(latex2exp)
#png('AnscombeQuartet220222c.png',width=16,height=12,units='cm',res=300)
par(mfrow=c(2,2))  # mf = multiple frames
# -----------------------------------------
plot(x1,y1,type='p',lwd=3,col='blue',
     xlab=TeX('$x_1$'),ylab=TeX('$y_1$'),
     las=1,cex=0.4,xlim=c(2,20),ylim=c(2,14))
out1 = lm(y1~x1)
yp0 = out1$coefficients[1]+out1$coefficients[2]*xp0
lines(xp0,yp0,col='black')
xmean = mean(x1); ymean = mean(y1)
xp1 = c(xmean,xmean); yp1 = c(0,20)
xp2 = c(-1,22); yp2 = c(ymean,ymean)
lines(xp1,yp1,col='red'); lines(xp2,yp2,col='red')
text(11,12,'+',col='red',cex=1)
text(7.4,12,'-',col='red',cex=1.5)
text(11,3,'-',col='red',cex=1.5)
text(7,3,'+',col='red',cex=1)
# -----------------------------------------
plot(x2,y2,type='p',lwd=3,col='blue',
     xlab=TeX('$x_2$'),ylab=TeX('$y_2$'),
     las=1,cex=0.4,xlim=c(2,20),ylim=c(2,14))
out2 = lm(y2~x2)
yp0 = out2$coefficients[1]+out2$coefficients[2]*xp0
lines(xp0,yp0,col='black')
xmean = mean(x2); ymean = mean(y2)
xp1 = c(xmean,xmean); yp1 = c(0,20)
xp2 = c(-1,22); yp2 = c(ymean,ymean)
lines(xp1,yp1,col='red'); lines(xp2,yp2,col='red')
text(11,12,'+',col='red',cex=1)
text(7.4,12,'-',col='red',cex=1.5)
text(11,3,'-',col='red',cex=1.5)
text(7,3,'+',col='red',cex=1)
# -----------------------------------------
plot(x3,y3,type='p',lwd=3,col='blue',
     xlab=TeX('$x_3$'),ylab=TeX('$y_3$'),
     las=1,cex=0.4,xlim=c(2,20),ylim=c(2,14))
out3 = lm(y3~x3)
yp0 = out3$coefficients[1]+out3$coefficients[2]*xp0
lines(xp0,yp0,col='black')
xmean = mean(x3); ymean = mean(y3)
xp1 = c(xmean,xmean); yp1 = c(0,20)
xp2 = c(-1,22); yp2 = c(ymean,ymean)
lines(xp1,yp1,col='red'); lines(xp2,yp2,col='red')
text(11,12,'+',col='red',cex=1)
text(7.4,12,'-',col='red',cex=1.5)
text(11,3,'-',col='red',cex=1.5)
text(7,3,'+',col='red',cex=1)
# -----------------------------------------
plot(x4,y4,type='p',lwd=3,col='blue',
     xlab=TeX('$x_4$'),ylab=TeX('$y_4$'),
     las=1,cex=0.4,xlim=c(2,20),ylim=c(2,14))
out4 = lm(y4~x4)
yp0 = out4$coefficients[1]+out4$coefficients[2]*xp0
lines(xp0,yp0,col='black')
xmean = mean(x4); ymean = mean(y4)
xp1 = c(xmean,xmean); yp1 = c(0,20)
xp2 = c(-1,22); yp2 = c(ymean,ymean)
lines(xp1,yp1,col='red'); lines(xp2,yp2,col='red')
text(11,12,'+',col='red',cex=1); text(7.4,12,'-',col='red',cex=1.5)
text(11,3,'-',col='red',cex=1.5); text(7,3,'+',col='red',cex=1)
# dev.off()