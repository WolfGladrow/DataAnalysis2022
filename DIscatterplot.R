# print('file: DIscatterplot.R')
# print(date())
# print('1 = scatter plot (8/2028)')
x = c(2.480, 0.668, 0.542, 1.770, 2.678, 2.931, 0.295, 4.656, 1.825)
# png('Data1Scatter220828.png',width=16,height=16,units='cm',res=300)
plot(x,type='p',lwd=4,col='blue',xlab='Data #',ylab='x',
     las=1,cex=0.6,cex.lab=1.5)
# dev.off()