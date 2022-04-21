print('file: TapeWormsTransparentBoxplot.R')
# Transparent boxplot of 2 data sets: tape worms
Y1=c(18,43,28,50,16,32,13,35,38,33,6,7)      # drugged
Y2=c(40,54,26,63,21,37,39,23,48,58,28,39)    # un-treated
# png('WormsTransparentBoxPlot220306.png',width=16,height=16,units='cm',res=300)
boxplot(Y1,Y2,col='yellow',las=1,xlab='Sample #',ylab='Number of worms',cex.lab=1.5)
set.seed(1953) # set seed for random number generators
L1 = length(Y1); jitter1 = runif(L1,1-0.05,1+0.05)
points(jitter1,Y1,col='blue',lwd=4,cex=0.6)
L2 = length(Y2); jitter2 = runif(L2,2-0.05,2+0.05)
points(jitter2,Y2,col='blue',lwd=4,cex=0.6)
# dev.off()