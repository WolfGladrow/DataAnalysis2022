print('file: EdelweissScatter1.R')
# Scatter plot: Edelweiss data
x = c(1853,1872,1899,1949,1976,1981,2001,2027,2033,2044,2111,2166,2245)
# png('EdelweissScatter171020.png',width=16,height=16,units='cm',res=300)
plot(x,type='p',lwd=4,col='blue',xlab='Data #',ylab='x (m)',las=1,cex=0.6,cex.lab=1.5)
# dev.off()
# ----------------------------------------------------------------
# Remarks:
# Everything behind the hash symbol '#' is comment and thus ignored by R. 
# If you want to activate command written right of a '#', just remove '#'; example:
# when you want to store the plot on your computer, remove the '#' in front of
# png() and dev.off().
# c()       concatenate: Combine Values into a Vector or List
# ?         ask for explanations
# ?c        ask for explanations for c()
# png()     define plot format; has to be called before calling plot()
# dev.off() shuts down all open graphics devices; has to be called to finish plot-file on your computer
# ----------------------------------------------------------------