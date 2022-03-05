print('file: UncertaintyDefs.R')
# plot measurand, measurements, error, ... 
# png('UncertaintyError161030.png',width=16,height=12,units='cm',res=300)
plot(1.5,1,type='p',col='blue',lwd=5,cex=0.6,xlim=c(-0.5,2.5),ylim=c(0,2),
     bty='n',col.axis='white',xaxt='n',yaxt='n',xlab='',ylab='')
text(1.5,1.55,'measurand',col='blue',cex=1.5)
text(1.5,1.35,'(true value)',col='blue',cex=1.5)
set.seed(1953) # set seed for random number generators
n1 = 5; r1 = rnorm(n=n1,mean=0.7,sd=1)
for(n in 1:n1) points(r1[n],0.75,col='red',lwd=1,cex=0.3)
points(mean(r1),0.75,col='red',lwd=5,cex=0.6)
n2 = 5; r2 = rnorm(n=n1,mean=0.7,sd=1.2)
for(n in 1:n2) points(r2[n],1.25,col='red',lwd=1,cex=0.3)
y2 = mean(r2);
y2 = 1;
points(mean(r2),1.25,col='red',lwd=5,cex=0.6)
text(0.7,1.7,'measurements',col='red',cex=1.5)
text(0.5,0.5,'repeatability',col='black',cex=1.5)
text(0.5,1.5,'reproducibility',col='black',cex=1.5)
# text(1.03,1.0,'< ----- error ----- >',col='black')
ye1 = 0.9
xe1 = (1.5+mean(r1))/2
text(xe1,1,'error',col='black',cex=1.5)
arrows(xe1,ye1,mean(r1),ye1,length = 0.15, angle = 15,col='red')
arrows(xe1,ye1,1.5,ye1,length = 0.15, angle = 15,col='blue')
ye2 = 1.1
xe2 = (1.5+mean(r2))/2
arrows(xe2,ye2,mean(r2),ye2,length = 0.15, angle = 15,col='red')
arrows(xe2,ye2,1.5,ye2,length = 0.15, angle = 15,col='blue')
x0=mean(r2); y0 = 1.25
arrows(x0,y0,x0+sd(r2),y0,length = 0.15, angle = 15)
arrows(x0,y0,x0-sd(r2),y0,length = 0.15, angle = 15)
x0=mean(r1); y0 = 0.75
arrows(x0,y0,x0+sd(r1),y0,length = 0.15, angle = 15)
arrows(x0,y0,x0-sd(r1),y0,length = 0.15, angle = 15)
# dev.off()
}