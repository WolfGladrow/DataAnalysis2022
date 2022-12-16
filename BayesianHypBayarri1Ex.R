print('file: BayesianHypBayarri1Ex.R')
print(date())
# Bayarri et al. (2008): Example 1
x = 0:3         # number of events
y = c(81,9,7,1) # frequency of events 
sflag = 1
if (sflag == 1) {
# png('Bayarri08Data1x191114.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='p',lwd=4,col='blue',xlab='Number of events',ylab='Frequency',las=1,cex=0.6,axes=F,cex.lab=1.5)
axis(side=1,labels=x,at=x)
yL = c(0,20,40,60,80)
axis(side=2,labels=yL,at=yL,las=1)
# dev.off()
}
# ----------------------------------------------------------------
# Remarks:
# if (sflag == 1) { plot() ...}   allows you to generate the plot with a single run command
# axes=F    suppresses plot of axes
# axis(side=1,labels=x,at=x)    x-axis with specified labels; here: integers (0.5 as label makes no sense)
# ----------------------------------------------------------------