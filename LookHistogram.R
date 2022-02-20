print('file: LookHistogram.R')
# data:
x1 = c(-0.68644, -0.82379, -0.98416, -2.02230, -0.43507, -0.76655,
       1.22178, 0.09767, -0.93391, -1.23458, 0.09188, 0.56736,
       -0.55276, -0.07969, 0.11767, 2.07541, 1.76443, 0.60249,
       -1.29916, -0.30322, -0.77935, -0.97190, 0.84580, 0.28698,
       1.15160, 0.35533, 0.32936, 1.68584, 0.18260, 1.93600)
# install.package('latex2exp')
library(latex2exp)
# png('x1PlotHist220220.png',width=16,height=12,units='cm',res=300)
par(mfrow=c(2,1)) # mf = multi frames
par(mai=c(1,1,0.25,1))
hist(x1,breaks=5,col='blue',xaxt='n',yaxt='n',xlab='',ylab='',main='',las=1)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5,las=1)
mtext(TeX('$x_1$'),side=1,line=3,cex=2)
mtext('Frequency',side=2,line=3,cex=2)
# hist(x1,breaks=7,col='blue',xaxt='n',yaxt='n',xlab='',ylab='',main='',las=1)
x1min = min(x1); x1max = max(x1); dx1 = (x1max-x1min)/7; 
V = seq(x1min,x1max,dx1);
hist(x1,breaks=V,col='blue',xaxt='n',yaxt='n',xlab='',ylab='',main='',las=1)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5,las=1)
mtext(TeX('$x_1$'),side=1,line=3,cex=2)
mtext('Frequency',side=2,line=3,cex=2)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# par()   set graphical parameters
# par(mfrow=c(2,1))   multi frame, 2 plots in 2 rows, 1 column
# par(mfrow=c(1,2))   multi frame, 2 plots in 1 row, 2 columns
# par(mai=c(bottom,left,top,right)) # margin size specified in inches
# breaks = n   a single number giving the number of cells for the histogram, however,
#              the number is a suggestion only; as the breakpoints will be set to pretty 
#              values
#              for n = 5 the number of bins is 6
#              for n = 7 the number of bins is 10 (including an empty one)
# if you want to specify an exact number of bin you should give a vector V of 
#    breakpoints between histogram cells, i.e. breaks = V where, for example,
#    x1min = min(x1); x1max = max(x1); dx1 = (x1max-x1min)/7; 
#    V = seq(x1min,x1max,dx1);
#    hist(x1,breaks=V,col='blue',xlab='x1',main='',las=1)