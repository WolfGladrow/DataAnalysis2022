print('file: TapeWormsBoxplot.R')
# Box plot of 2 data sets, list: tape worms
Y1=c(18,43,28,50,16,32,13,35,38,33,6,7)      # drugged
Y2=c(40,54,26,63,21,37,39,23,48,58,28,39)    # un-treated
# png('WormsBoxPlotList171021.png',width=16,height=16,units='cm',res=300)
boxplot(Y1,Y2,col='blue',las=1,xlab='',ylab='Number of worms',cex.lab=1.5)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# boxplot(Y1,Y2, ...)   two boxes
# ----------------------------------------------------------------
