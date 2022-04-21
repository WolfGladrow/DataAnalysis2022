print('file: TapeWormsScatter.R')
# Scatter plot of 2 data sets: tape worms
Y1=c(18,43,28,50,16,32,13,35,38,33,6,7)      # drugged
Y2=c(40,54,26,63,21,37,39,23,48,58,28,39)    # un-treated
ymax=max(Y1,Y2); Y1mean=mean(Y1); Y2mean = mean(Y2)
# png('WormsScatterPlot171021.png',width=16,height=16,units='cm',res=300)
plot(Y1,type='p',lwd=4,col='blue',xlab='Data #',
     ylab='Number of tape worms',las=1,cex=0.6,ylim=c(0,ymax+1),cex.lab=1.5)
points(Y2,col='black',lwd=4,cex=0.6,pch=24)
abline(Y1mean,0,col='blue',lty=2)
abline(Y2mean,0,col='black',lty=4)
# dev.off()